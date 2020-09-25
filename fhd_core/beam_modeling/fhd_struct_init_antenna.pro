FUNCTION fhd_struct_init_antenna,obs,beam_model_version=beam_model_version,$
    psf_resolution=psf_resolution,psf_intermediate_res=psf_intermediate_res,$
    psf_image_resolution=psf_image_resolution,timing=timing,$
    psf_dim=psf_dim,psf_max_dim=psf_max_dim,beam_offset_time=beam_offset_time,debug_dim=debug_dim,$
    inst_tile_ptr=inst_tile_ptr,ra_arr=ra_arr,dec_arr=dec_arr,fractional_size=fractional_size,$
    kernel_window=kernel_window,_Extra=extra
t0=Systime(1)

IF N_Elements(beam_model_version) EQ 0 THEN beam_model_version=1
instrument=obs.instrument
tile_gain_fn=instrument+'_beam_setup_gain' ;mwa_beam_setup_gain
tile_init_fn=instrument+'_beam_setup_init' ;mwa_beam_setup_init

;If other phases of the mwa are used, use the proper gain and init functions
IF STRMID(instrument,0,3) EQ 'mwa' THEN BEGIN
  tile_gain_fn='mwa_beam_setup_gain'
  tile_init_fn='mwa_beam_setup_init'
ENDIF

n_tiles=obs.n_tile
n_freq=obs.n_freq
n_pol=obs.n_pol
n_ant_pol=2 ;use as default, since almost all instruments have two instrumental polarizations (either linear or circular)

obsra=obs.obsra
obsdec=obs.obsdec
zenra=obs.zenra
zendec=obs.zendec
obsx=obs.obsx
obsy=obs.obsy
;phasera=obs.phasera
;phasedec=obs.phasedec
Jdate=obs.Jd0
IF Keyword_Set(beam_offset_time) THEN Jdate_use=Jdate+beam_offset_time/24./3600. ELSE Jdate_use=Jdate
frequency_array=(*obs.baseline_info).freq
freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1

tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
nbaselines=obs.nbaselines
ant_names=tile_A[uniq(tile_A[0:nbaselines-1],Sort(tile_A[0:nbaselines-1]))]


dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
;kx_span=kbinsize*dimension ;Units are # of wavelengths
;ky_span=kx_span
degpix=obs.degpix
astr=obs.astr

speed_light=299792458. ;speed of light, in meters/second
IF N_Elements(psf_resolution) EQ 0 THEN psf_resolution=16. ;=32? ;super-resolution factor
IF N_Elements(psf_image_resolution) EQ 0 THEN psf_image_resolution=10.
Eq2Hor,obsra,obsdec,Jdate_use,obsalt,obsaz,lat=obs.lat,lon=obs.lon,alt=obs.alt ; this may or may not include refraction
obsalt=Float(obsalt)
obsaz=Float(obsaz)
obsza=90.-obsalt

freq_center=fltarr(nfreq_bin)
FOR fi=0L,nfreq_bin-1 DO BEGIN
    fi_i=where(freq_bin_i EQ fi,n_fi)
    IF n_fi EQ 0 THEN freq_center[fi]=Interpol(frequency_array,freq_bin_i,fi) ELSE freq_center[fi]=Median(frequency_array[fi_i])
ENDFOR

;initialize antenna structure
antenna_str={n_pol:n_ant_pol,antenna_type:instrument,names:ant_names,model_version:beam_model_version,freq:freq_center,nfreq_bin:nfreq_bin,$
    n_ant_elements:0,Jones:Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin),coupling:Ptrarr(n_ant_pol,nfreq_bin),gain:Ptrarr(n_ant_pol),coords:Ptrarr(3),$
    delays:Ptr_new(),size_meters:0.,height:0.,response:Ptrarr(n_ant_pol,nfreq_bin),group_id:Lonarr(n_ant_pol)-1,pix_window:Ptr_new(),pix_use:Ptr_new(),$
    psf_image_dim:0.,psf_scale:0.}
    
;update structure with instrument-specific values, and return as a structure array, with an entry for each tile/antenna
;first, update to include basic configuration data
antenna=Call_function(tile_init_fn[0],obs,antenna_str,_Extra=extra) ;mwa_beam_setup_init
if N_elements(instrument) GT 1 then begin
  if ~isa(inst_tile_ptr, 'POINTER', /ARRAY) then message, 'instrument_tile_ptr must be filled in when there is more than one instrument. See dictionary.'
  for inst_i=1, N_elements(instrument)-1 do begin
    antenna_temp=Call_function(tile_init_fn[inst_i],obs,antenna_str,_Extra=extra)
    antenna[*inst_tile_ptr[inst_i]] = pointer_copy(antenna_temp[*inst_tile_ptr[inst_i]])
  endfor  
endif  
IF ~Keyword_Set(psf_dim) THEN $
    psf_dim=Ceil((Max(antenna.size_meters)*2.*Max(frequency_array)/speed_light)/kbinsize)
psf_dim=Ceil(psf_dim/2.)*2. ;dimension MUST be even
;reset psf_dim if cetain conditions met.
if keyword_set(debug_dim) then psf_dim=28.
if keyword_set(kernel_window) then psf_dim=18.

IF Keyword_Set(psf_max_dim) THEN BEGIN
    psf_max_dim=Ceil(psf_max_dim/2.)*2 ;dimension MUST be even
    IF psf_max_dim LT psf_dim THEN print,'Warning! PSF dim cut to '+Strn(psf_max_dim)+', fit dim was '+Strn(psf_dim)
    psf_dim=psf_dim<psf_max_dim
ENDIF

psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
psf_image_dim=psf_dim*psf_image_resolution*psf_intermediate_res ;use a larger box to build the model than will ultimately be used, to allow higher resolution in the initial image space beam model
psf_superres_dim=psf_dim*psf_resolution
psf_scale=dimension*psf_intermediate_res/psf_image_dim
antenna.psf_image_dim=psf_image_dim
antenna.psf_scale=psf_scale

xvals_celestial=meshgrid(psf_image_dim,psf_image_dim,1)*psf_scale-psf_image_dim*psf_scale/2.+obsx
yvals_celestial=meshgrid(psf_image_dim,psf_image_dim,2)*psf_scale-psf_image_dim*psf_scale/2.+obsy
;turn off refraction for speed, then make sure it is also turned off in Eq2Hor below
apply_astrometry, obs, x_arr=xvals_celestial, y_arr=yvals_celestial, ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad, /ignore_refraction
valid_i=where(Finite(ra_arr),n_valid)
ra_use=ra_arr[valid_i]
dec_use=dec_arr[valid_i]

;NOTE: Eq2Hor REQUIRES Jdate_use to have the same number of elements as RA and Dec for precession!!
;;NOTE: The NEW Eq2Hor REQUIRES Jdate_use to be a scalar! They created a new bug when they fixed the old one
Eq2Hor,ra_use,dec_use,Jdate_use,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1,/nutate, refract=0
za_arr=fltarr(psf_image_dim,psf_image_dim)+90. & za_arr[valid_i]=90.-alt_arr1
az_arr=fltarr(psf_image_dim,psf_image_dim) & az_arr[valid_i]=az_arr1

if keyword_set(kernel_window) then begin
  print, 'Applying a modified gridding kernel. Beam is no longer instrumental. Do not use for calibration.'

  ;Get pixels which fall within the horizon
  horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
  xvals_instrument=za_arr*Sin(az_arr*!DtoR)
  yvals_instrument=za_arr*Cos(az_arr*!DtoR)

  ;Calculate the phase center in x,y coords
  Eq2Hor,obs.orig_phasera,obs.orig_phasedec,Jdate_use,orig_phasealt,orig_phaseaz,lat=obs.lat,lon=obs.lon,alt=obs.alt
  apply_astrometry, obs, x_arr=xval_center, y_arr=yval_center, ra_arr=obs.orig_phasera, dec_arr=obs.orig_phasedec, /ad2xy
  xval_center = (90. - orig_phasealt)*sin(orig_phaseaz*!dtor)
  yval_center = (90. - orig_phasealt)*cos(orig_phaseaz*!dtor)
  
  ;Apply an image window to the pixels in the horizon, centered on the phase center
  pix_window = fltarr(psf_image_dim,psf_image_dim)
  if typename(kernel_window) NE 'STRING' then kernel_window='Blackman-Harris^2'
  pix_window[pix_use] = image_window(xvals_instrument[pix_use], yvals_instrument[pix_use], $
    image_window_name = kernel_window,xval_center=xval_center,yval_center=yval_center,fractional_size=fractional_size)
  ;Store image window in antenna structure
  antenna.pix_window=ptr_new(pix_window)
endif

if ~keyword_set(pix_use) then horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
antenna.pix_use=ptr_new(pix_use)

;now, update antenna structure to include gains
if N_elements(instrument) GT 1 then begin
  for inst_i=0, N_elements(instrument)-1 do begin
    antenna_temp = pointer_copy(antenna)
    antenna_temp=Call_function(tile_gain_fn[inst_i],obs,antenna_temp,za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,Jdate_use=Jdate_use,_Extra=extra) ;mwa_beam_setup_gain
    antenna[*inst_tile_ptr[inst_i]] = pointer_copy(antenna_temp[*inst_tile_ptr[inst_i]])
    antenna[*inst_tile_ptr[inst_i]].antenna_type = instrument[inst_i] ;if more than one instrument, assign the correct antenna type for each subset for metadata purposes
  endfor  
endif else antenna=Call_function(tile_gain_fn,obs,antenna,za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,Jdate_use=Jdate_use,_Extra=extra) ;mwa_beam_setup_gain

;Finally, update antenna structure to include the response of each antenna
antenna=general_antenna_response(obs,antenna,za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,_Extra=extra)

timing=Systime(1)-t0
RETURN,antenna
END
