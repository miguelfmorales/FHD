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

image_power_beam_arr = (*psf.image_info).image_power_beam_arr
psf_image_dim = (*psf.image_info).psf_image_dim
IF (size(image_power_beam_arr))[0] eq 1 then n_freq=1 else n_freq=(size(image_power_beam_arr))[2]
beam_base=Fltarr(psf_image_dim,psf_image_dim)
for fi=0,n_freq-1 do begin
    IF keyword_set(square) THEN BEGIN
        beam_base+=abs(*image_power_beam_arr[pol_i, fi])^2.
    ENDIF ELSE BEGIN
        beam_base+=abs(*image_power_beam_arr[pol_i, fi])
    ENDELSE
endfor
beam_base/=n_freq

IF N_Elements(dimension) EQ 0 THEN dimension=obs.dimension
obsx=obs.obsx
obsy=obs.obsy
psf_resolution = psf.resolution
psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
psf_scale=dimension*psf_intermediate_res/psf_image_dim
; Interpolate from
; x[ind]=ind*psf_scale-psf_image_dim*psf_scale/2.+obsx and y[ind]=ind*psf_scale-psf_image_dim*psf_scale/2.+obsy
xvals_interp = (findgen(fix(dimension))-obsx)/psf_scale + psf_image_dim/2.
yvals_interp = (findgen(fix(dimension))-obsy)/psf_scale + psf_image_dim/2.
beam_base = interpolate(beam_base, xvals_interp, yvals_interp, /grid, cubic=-0.5)

RETURN,beam_base
END
