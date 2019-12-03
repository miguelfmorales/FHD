;+
; :Description:
;    Generates the average beam image for one polarization
;
; :Params:
;    psf_base_ptr - equal to psf.base standard structure.
;
; :Keywords:
;    pol_i - polarization index. 0:XX, 1:YY, 2:XY, 3:YX
;    
;    freq_i - If set, returns the beam of a specific frequency bin instead of the average beam.
;    
;    dimension - size of image in pixels. If elements is also set, this refers to the size of the first dimension
;    
;    elements - defaults to elements=dimension
;
; :Author: isullivan May 4, 2012
;-
FUNCTION beam_image,psf,obs,pol_i=pol_i,freq_i=freq_i,dimension=dimension,elements=elements,abs=abs,square=square
compile_opt idl2,strictarrsubs  

IF N_Elements(pol_i) EQ 0 THEN pol_i=0

IF tag_exist(psf,'fbin_i') THEN freq_bin_i=psf.fbin_i

IF Keyword_Set(obs) THEN BEGIN
    IF Tag_exist(obs,'fbin_i') THEN freq_bin_i=obs.fbin_i ELSE freq_bin_i=(*obs.baseline_info).fbin_i
    n_freq=obs.n_freq
    IF tag_exist((*obs.baseline_info),'freq_use') THEN freq_i_use=where((*obs.baseline_info).freq_use GT 0) $
        ELSE freq_i_use=findgen(n_freq)
ENDIF

IF N_Elements(freq_i) GT 0 THEN freq_i_use=freq_i

image_power_beam_arr = (*psf.image_info).image_power_beam_arr
psf_image_dim = (*psf.image_info).psf_image_dim

IF N_Elements(freq_bin_i) EQ 0 THEN BEGIN
  n_freq=psf.n_freq
  freq_i_use = indgen(n_freq)
ENDIF ELSE BEGIN
  IF N_Elements(n_freq) EQ 0 THEN n_freq=N_Elements(freq_bin_i)
ENDELSE

beam_base=Fltarr(psf_image_dim,psf_image_dim)
for fi=0,n_freq-1 do begin
    freq_ind = freq_i_use[fi]
    IF keyword_set(square) THEN BEGIN
        beam_base+=image_power_beam_arr[pol_i, freq_ind]^2.
    ENDIF ELSE BEGIN
        beam_base+=image_power_beam_arr[pol_i, freq_ind]
    ENDELSE
endfor
beam_base/=n_freq

IF N_Elements(dimension) EQ 0 THEN dimension=obs.dimension
obsx=obs.obsx
obsy=obs.obsy
psf_resolution = psf.resolution
psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
psf_scale=dimension*psf_intermediate_res/psf_image_dim
; Interpolate from:
; x[ind]=ind*psf_scale-psf_image_dim*psf_scale/2.+obsx and y[ind]=ind*psf_scale-psf_image_dim*psf_scale/2.+obsy
; to x[ind]=ind-obsx and y[ind]=ind-obsy
xvals_interp = (findgen(fix(dimension))-2.*obsx)/psf_scale + psf_image_dim/2.
yvals_interp = (findgen(fix(dimension))-2.*obsy)/psf_scale + psf_image_dim/2.
beam_base = interpolate(beam_base, xvals_interp, yvals_interp, /grid, cubic=-0.5)

RETURN,beam_base
END
