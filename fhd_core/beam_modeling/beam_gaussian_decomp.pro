;;
;; Using input parameters p, build a gaussian mixture model of the image beam on 
;; the x,y grid. 
;;
;; x: Required x-axis vector
;; y: Required y-axis vector
;; p: Required gaussian parameter vector, 
;;  ordered as amp, offset x, sigma x, offset y, sigma y per lobe
;; fft: Optionally return the analytic Fourier Transform of the input gaussians
;; model_npix: Optionally provide the number of pixels on an axis used to derive
;;  the input parameters to convert to the current x,y grid 
;; model_res: Optionally provide the grid resolution used to derive the input
;;  parameters to convert to the current grid resolution
;;

function gaussian_decomp, x, y, p, fft=fft, model_npix=model_npix, model_res=model_res,$
  volume_beam=volume_beam,sq_volume_beam=sq_volume_beam,obs=obs,remove_first=remove_first,$
  remove_second=remove_second;,$
  
  nx = N_elements(x)
  ny = N_elements(y)
  decomp_beam=FLTARR(nx,ny)
  var = reform(p,5,N_elements(p)/5)  
  n_lobes = (size(var))[2]
  i = Complex(0,1)

  ;If the parameters were built on a different grid, then put on new grid
  ;Npix only affects the offset params
  ;Assumes model grid was smaller
  if keyword_set(model_npix) then begin
    offset = abs(nx/2. - model_npix/2.)
    var[1,*] = var[1,*] + offset 
    var[3,*] = var[3,*] + offset
  endif
  ;Res affects gaussian sigma and offsets
  if keyword_set(model_res) then begin
    var[2,*] = var[2,*] * model_res
    var[4,*] = var[4,*] * model_res
    var[1,*] = ((var[1,*]-nx/2.) * model_res) + nx/2.
    var[3,*] = ((var[3,*]-ny/2.) * model_res) + ny/2.
  endif
  
            
  if ~keyword_set(fft) then begin
    ;Full image model with all the gaussian components
    for lobe_i=0, n_lobes - 1 do begin
      decomp_beam += var[0,lobe_i] * $
        (exp(-(x-var[1,lobe_i])^2/(2*var[2,lobe_i]^2))#exp(-(y-var[3,lobe_i])^2/(2*var[4,lobe_i]^2)))  
    endfor
  endif else begin
    ;Full uv model with all the gaussian components
    if keyword_set(remove_first) then begin
      var[0,5]=0
      var[0,7]=0
      var[0,9]=0
      var[0,11]=0
    endif
    if keyword_set(remove_second) then begin
      var[0,6]=0
      var[0,8]=0
      var[0,10]=0
      var[0,12]=0
      var[0,13]=0
      var[0,14]=0
      var[0,15]=0
      var[0,16]=0
    endif
    volume_beam = total(var[0,*])
    sq_volume_beam = !Dpi*total((var[2,*])*(var[4,*])*var[0,*]^2)/(nx*ny)

    kx = (FINDGEN(nx)-nx/2.)#(FLTARR(ny)+1.)
    ky = (FLTARR(nx)+1.)#(FINDGEN(ny)-ny/2.)
    for lobe_i=0, n_lobes - 1 do begin 
      decomp_beam += var[0,lobe_i]*2.*!pi/(nx*ny)*var[2,lobe_i]*var[4,lobe_i]* $
        exp(-2*!pi^2/(nx*ny)*(var[2,lobe_i]^2*kx^2+var[4,lobe_i]^2*ky^2)-$
        2*!pi/(nx*ny)*i*(var[1,lobe_i]*kx+var[3,lobe_i]*ky))
    endfor
  endelse
  
  return, decomp_beam

end




pro beam_gaussian_decomp, image_power_beam, obs=obs, antenna1=antenna1, antenna2=antenna2, ant_pol1=ant_pol1,$
  ant_pol2=ant_pol2,psf_base_single=psf_base_single, maxiter=maxiter, beam_gaussian_params=beam_gaussian_params, $
  volume_beam=volume_beam,sq_volume_beam=sq_volume_beam,freq_i=freq_i,pol=pol,zen_int_x=zen_int_x,zen_int_y=zen_int_y,$
  remove_first=remove_first,remove_second=remove_second

  pix_use = *antenna1[0].pix_use
  n_freq = N_elements(antenna1.freq)
  pix_hor = obs.dimension/antenna1.psf_scale
  psf_image_dim = antenna1.psf_image_dim
  cen=pix_hor
  pad = 1.3
  x = FINDGEN(cen*pad)
  y = FINDGEN(cen*pad)
  range = [psf_image_dim/2-(cen*pad)/2.,psf_image_dim/2+(cen*pad)/2.-1]
  image_power_beam_use = image_power_beam


  ;; Create a functional form of the gaussian beams, with linear least squares fitting at selected frequencies
  ;;  Fitting every frequency would take too long *and* introduce spectral structure due to fitting residuals
  ;;  The YY polarization is just the flip of XX, so no need to refit.
  if (freq_i EQ 0) then begin

    if (pol EQ 0) then begin
      ;instrument-specific and pointing-specific gaussian parameters
      mwa_beam_gaussian_decomp, (cen*pad)/2., pix_hor, obs.kpix, parinfo=parinfo, parvalues=p, freq=antenna1.freq[freq_i], $
        gauss_beam_fbin = gauss_beam_fbin, pol=pol

      ;Least-squares iterator to fit gaussians
      if ~keyword_set(maxiter) then maxiter=100 ;default for MPFIT2DFUN is 200
      if gauss_beam_fbin GT n_freq then gauss_beam_fbin = n_freq
      for i=0, n_freq - 1 do if n_freq mod gauss_beam_fbin NE 0 then gauss_beam_fbin += 1.
      all_p = DBLARR(gauss_beam_fbin,N_elements(p))
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
      
      ;; Gaussian parameters constrained by selected frequencies. Fill in the rest of the freq with fitted polynomials
      if gauss_beam_fbin LT n_freq then begin
        poly_coeffs=FLTARR(N_elements(p),3)
        for p_i=0,N_elements(p)-1 do begin
          poly_coeffs[p_i,*] = poly_fit(FINDGEN(gauss_beam_fbin)*(n_freq/gauss_beam_fbin), all_p[*,p_i], 2)
          beam_gaussian_params[p_i,*] = $
            poly_coeffs[p_i,0] + poly_coeffs[p_i,1]*FINDGEN(n_freq) + poly_coeffs[p_i,2]*FINDGEN(n_freq)^2.
        endfor
      endif else beam_gaussian_params = transpose(all_p)

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
    fft=1,model_npix=cen*pad,volume_beam=volume_beam,sq_volume_beam=sq_volume_beam,obs=obs,$
    remove_first=remove_first,remove_second=remove_second)

end
