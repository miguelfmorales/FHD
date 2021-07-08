FUNCTION generate_source_cal_list,obs,psf,source_array,catalog_path=catalog_path,calibration_spectral_index=calibration_spectral_index,$
    max_calibration_sources=max_calibration_sources,calibration_flux_threshold=calibration_flux_threshold,$
    no_restrict_cal_sources=no_restrict_cal_sources,no_extend=no_extend,mask=mask,beam_cal_threshold=beam_cal_threshold,$
    allow_sidelobe_cal_sources=allow_sidelobe_cal_sources,beam_arr=beam_arr,model_visibilities=model_visibilities,$
    max_model_sources=max_model_sources,model_flux_threshold=model_flux_threshold,$
    no_restrict_model_sources=no_restrict_model_sources,model_spectral_index=model_spectral_index,$
    allow_sidelobe_model_sources=allow_sidelobe_model_sources,beam_model_threshold=beam_model_threshold,beam_threshold=beam_threshold,$
    preserve_zero_spectral_indices=preserve_zero_spectral_indices,flatten_spectrum=flatten_spectrum,$
    calibration_subtract_sidelobe_catalog=calibration_subtract_sidelobe_catalog,model_subtract_sidelobe_catalog=model_subtract_sidelobe_catalog,_Extra=extra

IF size(source_array,/type) EQ 8 THEN BEGIN
    catalog=source_array ;If a valid structure is supplied, use that 
ENDIF ELSE BEGIN
    UPNAME=StrUpCase(catalog_path)
    psav=strpos(UPNAME,'.SAV')>strpos(UPNAME,'.IDLSAVE')
    IF psav EQ -1 THEN catalog_path+='.sav'
    IF file_test(catalog_path) EQ 0 THEN BEGIN
        catalog_path_full=filepath(catalog_path,root=Rootdir('fhd'),subdir='catalog_data')
        IF file_test(catalog_path_full) EQ 0 THEN BEGIN
            print, catalog_path+' not found! Critical problem, quitting!'
            exit
        ENDIF
    ENDIF ELSE catalog_path_full=catalog_path

    catalog = load_source_catalog(catalog_path_full, varname='catalog')
ENDELSE

dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix

FoV=!RaDeg/obs.kpix
freq_arr=psf.freq
freq_use=obs.freq_center
IF freq_use GT 1E5 THEN freq_use/=1E6
n_pol=obs.n_pol
    
IF N_Elements(beam_arr) LT (n_pol<2) THEN BEGIN 
    beam_arr=Ptrarr(n_pol<2)
    FOR pol_i=0,(n_pol<2)-1 DO beam_arr[pol_i]=Ptr_new(beam_image(psf,obs,pol_i=pol_i,square=0)>0.)
ENDIF
beam=fltarr(dimension,elements)
FOR pol_i=0,(n_pol<2)-1 DO beam+=*beam_arr[pol_i]^2.
beam=Sqrt(beam/(n_pol<2))

IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=0.05
no_restrict_sources_default = 1

IF Keyword_Set(model_visibilities) THEN BEGIN
    IF N_Elements(model_flux_threshold) GT 0 THEN flux_threshold=model_flux_threshold
    IF N_Elements(allow_sidelobe_model_sources) GT 0 THEN allow_sidelobe_sources=allow_sidelobe_model_sources ELSE allow_sidelobe_sources=0
    IF Keyword_Set(allow_sidelobe_sources) THEN IF N_Elements(no_restrict_model_sources) EQ 0 THEN no_restrict_sources=1 ELSE no_restrict_sources=no_restrict_model_sources $
        ELSE no_restrict_sources=no_restrict_sources_default
    IF N_Elements(max_model_sources) GT 0 THEN max_sources=max_model_sources ELSE max_sources=0 ;0 turns it off
    IF N_Elements(model_spectral_index) GT 0 THEN spectral_index=model_spectral_index ELSE spectral_index=catalog.alpha
    IF Keyword_Set(allow_sidelobe_sources) THEN beam_threshold=0.01
    IF N_Elements(beam_model_threshold) GT 0 THEN beam_threshold=beam_model_threshold
ENDIF ELSE BEGIN
    IF N_Elements(calibration_flux_threshold) GT 0 THEN flux_threshold=calibration_flux_threshold
    IF N_Elements(allow_sidelobe_cal_sources) GT 0 THEN allow_sidelobe_sources=allow_sidelobe_cal_sources ELSE allow_sidelobe_sources=0
    IF Keyword_Set(allow_sidelobe_sources) THEN IF N_Elements(no_restrict_cal_sources) EQ 0 THEN no_restrict_sources=1 ELSE no_restrict_sources=no_restrict_cal_sources $
        ELSE no_restrict_sources=no_restrict_sources_default
    IF N_Elements(max_calibration_sources) GT 0 THEN max_sources=max_calibration_sources ELSE max_sources=0 ;0 turns it off
    IF N_Elements(calibration_spectral_index) GT 0 THEN spectral_index=calibration_spectral_index ELSE spectral_index=catalog.alpha
    IF Keyword_Set(allow_sidelobe_sources) THEN beam_threshold=0.01
    IF N_Elements(beam_cal_threshold) GT 0 THEN beam_threshold=beam_cal_threshold
ENDELSE

obs_i = Round(obs.obsx) + dimension*Round(obs.obsy)
beam_primary_i=region_grow(beam,obs_i,threshold=[Max(beam)/2.<beam_threshold,Max(beam)>1.])
beam_primary_mask=fltarr(dimension,elements) & beam_primary_mask[beam_primary_i]=1.
beam_sidelobe_mask = 1 - beam_primary_mask
IF Keyword_Set(model_visibilities) THEN BEGIN
    IF N_Elements(model_subtract_sidelobe_catalog) GT 0 THEN BEGIN
        IF N_Elements(no_restrict_model_sources) EQ 0 THEN no_restrict_sources=no_restrict_sources_default $
            ELSE no_restrict_sources=no_restrict_model_sources
        allow_sidelobe_sources=0
        sidelobe_catalog = generate_source_cal_list(obs,psf,catalog_path=model_subtract_sidelobe_catalog,$
            mask=beam_sidelobe_mask,/allow_sidelobe_model_sources,/model_visibilities,$
            beam_arr=beam_arr, no_extend=no_extend,$
            max_model_sources=max_model_sources,model_flux_threshold=model_flux_threshold,$
            no_restrict_model_sources=no_restrict_model_sources,model_spectral_index=model_spectral_index,$
            beam_model_threshold=beam_model_threshold,$
            preserve_zero_spectral_indices=preserve_zero_spectral_indices,flatten_spectrum=flatten_spectrum,_Extra=extra)
    ENDIF
ENDIF ELSE BEGIN
    IF N_Elements(calibration_subtract_sidelobe_catalog) GT 0 THEN BEGIN
        IF N_Elements(no_restrict_model_sources) EQ 0 THEN no_restrict_sources=no_restrict_sources_default $
            ELSE no_restrict_sources=no_restrict_model_sources
        allow_sidelobe_sources=0
        sidelobe_catalog = generate_source_cal_list(obs,psf,catalog_path=calibration_subtract_sidelobe_catalog,$
            mask=beam_sidelobe_mask,/allow_sidelobe_cal_sources,$
            calibration_spectral_index=calibration_spectral_index,beam_arr=beam_arr,$
            max_calibration_sources=max_calibration_sources,calibration_flux_threshold=calibration_flux_threshold,$
            no_restrict_cal_sources=no_restrict_cal_sources,no_extend=no_extend,beam_cal_threshold=beam_cal_threshold,$
            delicate_calibration_catalog=delicate_calibration_catalog,$
            preserve_zero_spectral_indices=preserve_zero_spectral_indices,flatten_spectrum=flatten_spectrum,_Extra=extra)
    ENDIF
ENDELSE

IF Keyword_Set(no_restrict_sources) THEN fft_alias_range=dimension/32. ELSE fft_alias_range=dimension/4.

ra0=obs.obsra
dec0=obs.obsdec
angs=angle_difference(dec0,ra0,catalog.dec,catalog.ra,/degree)
i_use=where(Abs(angs) LE FoV/2.,n_use)

IF N_Elements(spectral_index) GT 1 AND (not Keyword_Set(preserve_zero_spectral_indices)) THEN BEGIN
    zero_i=where(spectral_index EQ 0,n_zero,complement=nonzero_i,ncomplement=n_nonzero)
    IF n_zero GT 0 THEN BEGIN
        IF n_nonzero GT 5 THEN Resistant_mean,spectral_index[nonzero_i],2,alpha_mean ELSE alpha_mean=-0.8
        spectral_index[zero_i]=alpha_mean
    ENDIF
ENDIF

IF n_use GT 0 THEN BEGIN
    catalog=catalog[i_use]
    IF N_Elements(spectral_index) GT 1 THEN spectral_index=spectral_index[i_use]
    if tag_exist(catalog, 'shape') then begin
      source_list=source_comp_init(n_sources=n_use,freq=freq_use,ra=catalog.ra,dec=catalog.dec,$
        alpha=spectral_index,extend=catalog.extend,shape=catalog.shape)
    endif else begin
      source_list=source_comp_init(n_sources=n_use,freq=freq_use,ra=catalog.ra,dec=catalog.dec,$
        alpha=spectral_index,extend=catalog.extend)
    endelse
   
    apply_astrometry, obs, ra_arr=source_list.ra, dec_arr=source_list.dec, x_arr=x_arr, y_arr=y_arr, /ad2xy    
    source_list.x=x_arr
    source_list.y=y_arr
    FOR i=0,7 DO source_list.flux.(i)=catalog.flux.(i)*(freq_use/catalog.freq)^spectral_index
    source_list.alpha=spectral_index

    IF Keyword_Set(allow_sidelobe_sources) THEN beam_i=where(beam GT beam_threshold) $
        ELSE beam_i=region_grow(beam,dimension/2.+dimension*elements/2.,threshold=[Max(beam)/2.<beam_threshold,Max(beam)>1.])
    beam_mask=fltarr(dimension,elements) & beam_mask[beam_i]=1.
    IF N_Elements(mask) EQ N_Elements(beam_mask) THEN beam_mask*=mask
    
    ;If flux_threshold is negative, assume that it is an UPPER bound, and only include the fainter sources
    flux_I_use = source_list.flux.I
    IF N_elements(flux_threshold) NE 0 THEN BEGIN
        IF flux_threshold LT 0 THEN flux_I_use = -flux_I_use
        src_use=where((x_arr GE fft_alias_range) AND (x_arr LE dimension-1-fft_alias_range) AND (y_arr GE fft_alias_range) $
            AND (y_arr LE elements-1-fft_alias_range) AND (flux_I_use GT flux_threshold) AND (flux_I_use NE 0),n_src_use)
    ENDIF ELSE BEGIN
        src_use=where((x_arr GE fft_alias_range) AND (x_arr LE dimension-1-fft_alias_range) AND (y_arr GE fft_alias_range) $
            AND (y_arr LE elements-1-fft_alias_range) AND (flux_I_use NE 0),n_src_use)
    ENDELSE    

    IF n_src_use EQ 0 THEN RETURN,source_comp_init(n_sources=0,freq=obs.freq_center);
    src_use2=where(beam_mask[Round(x_arr[src_use]),Round(y_arr[src_use])],n_src_use)
    IF n_src_use GT 0 THEN src_use=src_use[src_use2]
    source_list=source_list[src_use]
    inds_finite = where(finite(source_list.flux.I[*]),n_finite)
    IF n_finite NE N_elements(src_use) then begin
        print, "WARNING: Model catalog contains nan/inf fluxes"
        source_list=source_list[inds_finite]
        n_src_use=n_finite
    ENDIF
    influence=source_list.flux.I*beam[source_list.x,source_list.y]
   
    order=Reverse(sort(influence))
    source_list=source_list[order]
    source_list.id=Lindgen(n_src_use)
    IF Keyword_Set(no_extend) THEN source_list.extend=Ptrarr(n_src_use) ELSE BEGIN
        extend_i=where(Ptr_valid(source_list.extend),n_extend)
        FOR ext_i=0L,n_extend-1 DO BEGIN
            extend_list=*source_list[extend_i[ext_i]].extend
            apply_astrometry, obs, ra_arr=extend_list.ra, dec_arr=extend_list.dec, x_arr=x_arr, y_arr=y_arr, /ad2xy
            extend_list.x=x_arr
            extend_list.y=y_arr
            *source_list[extend_i[ext_i]].extend=extend_list
        ENDFOR
    ENDELSE
ENDIF ELSE RETURN,source_comp_init(n_sources=0,freq=obs.freq_center)

IF Keyword_Set(sidelobe_catalog) THEN $
    source_list = source_list_append(obs,source_list,sidelobe_catalog)

IF Keyword_Set(max_sources) THEN IF N_Elements(source_list) GT max_sources $
    THEN source_list=source_list[0:max_sources-1]

IF Keyword_Set(flatten_spectrum) THEN BEGIN
    alpha_avg=Total(source_list.alpha*(source_list.flux.I>0))/Total(source_list.flux.I>0)
    obs.alpha=alpha_avg
    source_list.alpha-=alpha_avg
ENDIF
RETURN,source_list
END

