;;
;; Decompose the image beam of an instrument using pre-defined gaussians in a 
;;  2D-fitting routine.
;;
;; image_power_beam: Required image of the beam power
;; obs: Required obs structure for observation info
;; antenna1, antenna2: Required antenna structures for metadata and responses 
;; ant_pol1, ant_pol2: Required polarization of the antennas
;; psf_base_single: Returns the gaussian decomposed uv beam for the two antennas 
;; maxiter: Option to control the maximum number of iterations for the 2D fitter
;; beam_gaussian_params: Returns the gaussian parameters fit with the 2D fitter
;; volume_beam: Returns the analytical calculation of the beam volume
;; sq_volume_beam: Returns the analytical calculation of the squared beam volume
;; freq_i: Required frequency index for the current fit
;; pol: Required polarization index for the current_fit
;; zen_int_x, zen_int_y: Pixel indicies that line up with a common grid
;; beam_gauss_param_transfer: Optionally transfer a psf structure with params instead of fitting 


pro beam_gaussian_decomp, image_power_beam, obs=obs, antenna1=antenna1, antenna2=antenna2, ant_pol1=ant_pol1,$
  ant_pol2=ant_pol2,psf_base_single=psf_base_single, maxiter=maxiter, beam_gaussian_params=beam_gaussian_params, $
  volume_beam=volume_beam,sq_volume_beam=sq_volume_beam,freq_i=freq_i,pol=pol,zen_int_x=zen_int_x,zen_int_y=zen_int_y,$
  beam_gauss_param_transfer=beam_gauss_param_transfer

  instrument=obs.instrument
  ;All phases of the MWA use the same beam parameters
  if STRMID(instrument,0,3) eq 'mwa' then begin
    beam_decomp_fn = 'mwa_beam_gaussian_decomp'
  endif else beam_decomp_fn = instrument + '_beam_gaussian_decomp'

  pix_use = *antenna1[0].pix_use ;non-zero pixel indicies of the beam
  n_freq = N_elements(antenna1.freq)
  pix_hor = obs.dimension/antenna1.psf_scale ;number of pixels spanning horizon to horizon
  psf_image_dim = antenna1.psf_image_dim
  cen=pix_hor
  pad = 1.3 ;slight image padding factor to include zeroed super-horizon pixels for fitting purposes
  x = FINDGEN(cen*pad)
  y = FINDGEN(cen*pad)
  range = [psf_image_dim/2-(cen*pad)/2.,psf_image_dim/2+(cen*pad)/2.-1]
  image_power_beam_use = image_power_beam


  ;; Create a functional form of the gaussian beams, with linear least squares fitting at selected frequencies
  ;;  Fitting every frequency would take too long *and* introduce spectral structure due to fitting residuals
  ;;  The YY polarization is just the flip of XX, so no need to refit.
  if (freq_i EQ 0) then begin

    if (pol EQ 0) then begin

      ;optionally transfer pre-fitted gaussian parameters from a psf structure
      if keyword_set(beam_gauss_param_transfer) then begin
        psf_transfer = getvar_savefile(beam_gauss_param_transfer,'psf')
        beam_gaussian_params = *psf_transfer.beam_gaussian_params[0]
      endif else begin

        ;instrument-specific and pointing-specific gaussian parameters (calls mwa_beam_gaussian_decomp for the mwa)
        ;mwa_beam_gaussian_decomp, (cen*pad)/2., pix_hor, obs, parinfo=parinfo, parvalues=p, freq=antenna1.freq[freq_i], $
        ;  gauss_beam_fbin = gauss_beam_fbin, pol=pol
        Call_procedure, beam_decomp_fn, (cen*pad)/2., pix_hor, obs, parinfo=parinfo, parvalues=p, freq=antenna1.freq[freq_i], $
          gauss_beam_fbin = gauss_beam_fbin, pol=pol

        ;; Least-squares iterator to fit gaussians
        ;;  Loop through a subset of frequencies determined by the instrument in gauss_beam_fbin for speed
        ;;  and spectral smoothness, then fit a polynomial across the params to fill in the rest of the frequencies
        if ~keyword_set(maxiter) then maxiter=100 ;default for MPFIT2DFUN is 200
        if gauss_beam_fbin GT n_freq then gauss_beam_fbin = n_freq
        for i=0, n_freq - 1 do if n_freq mod gauss_beam_fbin NE 0 then gauss_beam_fbin += 1.
        all_p = DBLARR(gauss_beam_fbin,N_elements(p)) ;array for all of the gaussian parameters
        for fbin_i=0, gauss_beam_fbin-1 do begin
          fbin = (n_freq / gauss_beam_fbin) * fbin_i
        
          ;create image power beam at specific frequency
          beam_ant1=DComplexarr(psf_image_dim,psf_image_dim)
          beam_ant2=DComplexarr(psf_image_dim,psf_image_dim)
          beam_ant1[pix_use]=DComplex(*(antenna1.response[ant_pol1,fbin]))
          beam_ant2[pix_use]=DComplex(Conj(*(antenna2.response[ant_pol2,fbin])))
          Jones1=antenna1.Jones[*,*,fbin]
          Jones2=antenna2.Jones[*,*,fbin]

          ;Amplitude of the response from ant1 is Sqrt(|J1[0,pol1]|^2 + |J1[1,pol1]|^2)
          ;Amplitude of the response from ant2 is Sqrt(|J2[0,pol2]|^2 + |J2[1,pol2]|^2)
          ;Amplitude of the baseline response is the product of the antenna responses
          power_zenith_beam=Dcomplexarr(psf_image_dim,psf_image_dim)
          power_zenith_beam[pix_use]=Sqrt((abs(*Jones1[0,ant_pol1])^2.+abs(*Jones1[1,ant_pol1])^2.)*$
            (abs(*Jones2[0,ant_pol2])^2.+abs(*Jones2[1,ant_pol2])^2.))
          power_zenith=Interpolate(power_zenith_beam,zen_int_x,zen_int_y,cubic=-0.5)
          power_beam = power_zenith_beam*beam_ant1*beam_ant2

          image_power_beam_use=power_beam/power_zenith

          ;; Fit the gaussian decomposition to the instrumental beam image using the 2D fitter
          ;;  p are the input params, parinfo is a structure which details constraints on p, weights are set to one by default,
          ;;  no covariance information is default, and chi_squared and niter detail the statistics of the fit 
          t0=Systime(1)
          fitted_p = MPFIT2DFUN('gaussian_decomp', x, y, $
            abs(image_power_beam_use[range[0]:range[1],range[0]:range[1]]), 1 , p, parinfo=parinfo, weights=1d, /quiet, errmsg=errmsg, $
            maxiter=maxiter,nocovar=1,bestnorm=chi_squared,niter=niter)
          timing=Systime(1)-t0
          print, "Chi-squared of beam gaussian fit is " + strtrim(chi_squared,2) + " in " + strtrim(niter,2) + $
            " total iterations for " + strtrim(timing,2) + 'secs'
          if keyword_set(errmsg) then message, "Gaussian mixture model least-squares fitting return error: " + errmsg

          all_p[fbin_i,*] = fitted_p 

          ;Update estimate of gauss params for next iteration
          p=fitted_p
        endfor

        if ~keyword_set(beam_gaussian_params) then beam_gaussian_params = DBLARR(N_elements(p),n_freq)
      
        ;; Gaussian parameters constrained by selected frequencies. Fill in the rest of the freq with second-order polynomial
        if gauss_beam_fbin LT n_freq then begin
          poly_coeffs=FLTARR(N_elements(p),3)
          for p_i=0,N_elements(p)-1 do begin
            poly_coeffs[p_i,*] = poly_fit(FINDGEN(gauss_beam_fbin)*(n_freq/gauss_beam_fbin), all_p[*,p_i], 2)
            beam_gaussian_params[p_i,*] = $
              poly_coeffs[p_i,0] + poly_coeffs[p_i,1]*FINDGEN(n_freq) + poly_coeffs[p_i,2]*FINDGEN(n_freq)^2.
          endfor
        endif else beam_gaussian_params = transpose(all_p)

      endelse ;end else transfer

    endif else begin
    ;if YY polarization, then flip the fitted XX
      var = reform(beam_gaussian_params,5,N_elements(beam_gaussian_params[*,0])/5.,n_freq)    
      temp = var[1,*,*]
      var[1,*,*] = var[3,*,*]
      var[3,*,*] = temp
      temp = var[2,*,*]
      var[2,*,*] = var[4,*,*]
      var[4,*,*] = temp
      beam_gaussian_params = reform(var,N_elements(beam_gaussian_params[*,0]),n_freq)
    endelse

  endif

  ;Build uv-plane of the fitted gaussians
  psf_base_single = gaussian_decomp(FINDGEN(psf_image_dim),FINDGEN(psf_image_dim),beam_gaussian_params[*,freq_i],$
    fft=1,model_npix=cen*pad,volume_beam=volume_beam,sq_volume_beam=sq_volume_beam)

end
