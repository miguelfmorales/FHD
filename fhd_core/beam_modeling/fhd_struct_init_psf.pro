;+
; :Description:
;    Initializes the structure containing gridded PSF data for a visibility.

; :Author: isullivan May 6, 2012
;-
FUNCTION fhd_struct_init_psf,beam_ptr=beam_ptr,complex_flag=complex_flag,$
    xvals=xvals,yvals=yvals,fbin_i=fbin_i,psf_resolution=psf_resolution,psf_dim=psf_dim,$
    n_pol=n_pol,n_freq=n_freq,freq_cen=freq_cen,pol_norm=pol_norm,freq_norm=freq_norm,group_arr=group_arr,$
    interpolate_kernel=interpolate_kernel,beam_mask_threshold=beam_mask_threshold,$
    image_power_beam_arr=image_power_beam_arr,ra_arr=ra_arr,dec_arr=dec_arr,$
    psf_image_resolution=psf_image_resolution,psf_image_dim=psf_image_dim,$
    beam_gaussian_params_arr=beam_gaussian_params_arr,pix_horizon=pix_horizon 
 
IF N_Elements(n_pol) EQ 0 THEN n_pol=1 ELSE n_pol=Fix(n_pol)
IF N_Elements(n_freq) EQ 0 THEN n_freq=1 ELSE n_freq=Fix(n_freq)
IF N_Elements(freq_cen) EQ 0 THEN freq_cen=Fltarr(n_freq) ELSE freq_cen=Float(freq_cen)
IF N_Elements(beam_ptr) EQ 0 THEN beam_ptr=Ptr_new() ;will actually have dimensions (npol,nfreq,nbaselines)
psf_resolution=Fix(psf_resolution);over-resolution
IF N_Elements(psf_dim) EQ 0 THEN psf_dim=1L ELSE psf_dim=Long(psf_dim)
IF N_Elements(xvals) EQ 0 THEN xvals=Ptrarr(1) ;will have dimensions of (resolution,resolution)
IF N_Elements(pol_norm) EQ 0 THEN pol_norm=replicate(1.,n_pol) ELSE pol_norm=Float(pol_norm)
IF N_Elements(freq_norm) EQ 0 THEN freq_norm=replicate(1.,n_freq) ELSE freq_norm=Float(freq_norm)
IF N_Elements(fbin_i) EQ 0 THEN fbin_i=Lonarr(1) ELSE fbin_i=Long(fbin_i)
IF N_Elements(complex_flag) EQ 0 THEN complex_flag=1
IF N_Elements(group_arr) EQ 0 THEN group_arr=Lonarr(size(beam_arr,/dimension))
IF N_Elements(interpolate_kernel) EQ 0 THEN interpolate_kernel=0 ELSE interpolate_kernel=Keyword_Set(interpolate_kernel)
IF N_Elements(beam_mask_threshold) EQ 0 THEN beam_mask_threshold=0

IF N_Elements(image_power_beam_arr) EQ 0 THEN image_power_beam_arr=Ptr_new()
IF N_Elements(ra_arr) EQ 0 THEN ra_arr=0
IF N_Elements(dec_arr) EQ 0 THEN dec_arr=0
IF N_Elements(psf_image_resolution) EQ 0 THEN psf_image_resolution=0
IF N_Elements(psf_image_dim) EQ 0 THEN psf_image_dim=0

IF N_Elements(beam_gaussian_params_arr) EQ 0 THEN beam_gaussian_params_arr=0

arr={image_power_beam_arr:image_power_beam_arr,ra_arr:ra_arr,dec_arr:dec_arr,$
    psf_image_resolution:psf_image_resolution,psf_image_dim:psf_image_dim}
struct={beam_ptr:beam_ptr,xvals:xvals,yvals:yvals,pnorm:pol_norm,fnorm:freq_norm,id:group_arr,$
    fbin_i:fbin_i,resolution:psf_resolution,dim:psf_dim,complex_flag:complex_flag,n_pol:n_pol,$
    n_freq:n_freq,freq:freq_cen,interpolate_kernel:interpolate_kernel,$
    beam_mask_threshold:beam_mask_threshold,image_info:Ptr_new(arr),$
    beam_gaussian_params:beam_gaussian_params_arr,pix_horizon:pix_horizon}

RETURN,struct
END
