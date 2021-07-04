FUNCTION vis_calibrate,vis_ptr,cal,obs,status_str,psf,params,jones,vis_weight_ptr=vis_weight_ptr,model_uv_arr=model_uv_arr,$
    transfer_calibration=transfer_calibration,timing=timing,file_path_fhd=file_path_fhd,$
    n_cal_iter=n_cal_iter,error=error,preserve_visibilities=preserve_visibilities,$
    debug=debug,gain_arr_ptr=gain_arr_ptr,calibration_flag_iterate=calibration_flag_iterate,$
    return_cal_visibilities=return_cal_visibilities,silent=silent,initial_calibration=initial_calibration,$
    calibration_visibilities_subtract=calibration_visibilities_subtract,vis_baseline_hist=vis_baseline_hist,$
    flag_calibration=flag_calibration,vis_model_arr=vis_model_arr,$
    calibration_auto_fit=calibration_auto_fit,cal_stop=cal_stop, model_transfer=model_transfer,$
    sim_over_calibrate=sim_over_calibrate,sim_perf_calibrate=sim_perf_calibrate,$
    auto_ratio_calibration=auto_ratio_calibration,no_png=no_png,_Extra=extra
    
  t0_0=Systime(1)
  error=0
  timing=-1
  initialize_fhd_struct, 'cal', obs=obs, params=params
  heap_gc
  IF N_Elements(flag_calibration) EQ 0 THEN flag_calibration=1
  
  IF Keyword_Set(transfer_calibration) THEN BEGIN
    IF size(transfer_calibration,/type) EQ 7 THEN BEGIN
      cal_file_use=transfer_calibration
      IF file_test(cal_file_use,/directory) THEN BEGIN
        fhd_save_io,file_path_fhd=file_path_fhd,transfer=transfer_calibration,var='cal',path_use=cal_file_use2,_Extra=extra
        cal_file_use2+='.sav'
        IF file_test(cal_file_use2) THEN cal_file_use=cal_file_use2 ELSE BEGIN
          print,'File:'+cal_file_use2+' not found!'
          error=1
          RETURN,vis_ptr
        ENDELSE
      ENDIF ELSE BEGIN
        IF file_test(cal_file_use) EQ 0 THEN BEGIN
          fhd_save_io,file_path_fhd=cal_file_use,var='cal',path_use=cal_file_use2,_Extra=extra
          cal_file_use2+='.sav'
          IF file_test(cal_file_use2) THEN cal_file_use=cal_file_use2 ELSE BEGIN
            print,'File:'+cal_file_use2+' not found!'
            error=1
            RETURN,vis_ptr
          ENDELSE
        ENDIF
      ENDELSE
      CASE StrLowCase(Strmid(cal_file_use[0],3,/reverse)) OF
        '.sav':BEGIN
          cal=getvar_savefile(cal_file_use,'cal')
          gain_arr_ptr=cal.gain
          IF ~Keyword_Set(cal.cal_origin) THEN cal.cal_origin=cal_file_use
          ;cal=fhd_struct_init_cal(obs,params,calibration_origin=cal.cal_origin,gain_arr_ptr=cal.gain,_Extra=extra)
        END
        '.txt':BEGIN
          textfast,gain_arr,/read,file_path=cal_file_use
          gain_arr_ptr=Ptr_new(gain_arr)
          cal=fhd_struct_init_cal(obs,params,calibration_origin=cal_file_use,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
        END
        '.npz':BEGIN
          gain_arr=read_numpy(cal_file_use)
          gain_arr_ptr=Ptr_new(gain_arr)
          cal=fhd_struct_init_cal(obs,params,calibration_origin=cal_file_use,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
        END
        '.npy':BEGIN
          gain_arr=read_numpy(cal_file_use)
          gain_arr_ptr=Ptr_new(gain_arr)
          cal=fhd_struct_init_cal(obs,params,calibration_origin=cal_file_use,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
        END
        'fits':BEGIN ;calfits format
          cal = calfits_read(cal_file_use,obs,params,silent=silent,_Extra=extra)
        END
        ELSE: BEGIN
          print,'Unknown file format: ',cal_file_use
          error=1
          RETURN,vis_ptr
        ENDELSE
      ENDCASE
    ENDIF

    vis_cal=vis_calibration_apply(vis_ptr,cal)
    print,'Calibration transferred from ' + cal_file_use
    timing=Systime(1)-t0_0

    ;;Check that the flagging of the restored cal is accurately captured in the obs structure
    ;; and broadcast flags across all pols  
    for pol_i=0,cal.n_pol-1 do begin
      tile_total=FLTARR(cal.n_tile)
      freq_total=FLTARR(cal.n_freq)
      for tile_i=0, cal.n_tile-1 do tile_total[tile_i] = total(abs((*cal.gain[pol_i])[*,tile_i]),/NAN)
      for freq_i=0, cal.n_freq-1 do freq_total[freq_i] = total(abs((*cal.gain[pol_i])[freq_i,*]),/NAN)
      tile_flag_inds = where(tile_total EQ 0,n_tile_flag)                          
      freq_flag_inds = where(freq_total EQ 0,n_freq_flag)
      if n_tile_flag GT 0 then begin
        (*obs.baseline_info).tile_use[tile_flag_inds]=0
        temp = where((*obs.baseline_info).tile_use EQ 0,n_tile_flag)
        obs.n_tile_flag = n_tile_flag
      endif
      if n_freq_flag GT 0 then begin
        (*obs.baseline_info).freq_use[freq_flag_inds]=0
        temp = where((*obs.baseline_info).freq_use EQ 0,n_freq_flag)
        obs.n_freq_flag = n_freq_flag
      endif
    endfor

    ;; Option to transfer pre-made and unflagged model visbilities
    if keyword_set(model_transfer) then begin
      vis_model_arr=PTRARR(obs.n_pol,/allocate)
      
      for pol_i=0, obs.n_pol-1 do begin
        transfer_name = model_transfer + '/' + obs.obsname + '_vis_model_'+obs.pol_names[pol_i]+'.sav'
        if ~file_test(transfer_name) then begin
          message, transfer_name + ' not found during model transfer.'
        endif
        vis_model_arr[pol_i] = getvar_savefile(transfer_name,'vis_model_ptr')
      endfor
    endif

    RETURN,vis_cal
  ENDIF
  
  if ~keyword_set(model_transfer) then begin
    vis_model_arr=vis_source_model(cal.skymodel,obs,status_str,psf,params,vis_weight_ptr,cal,jones,$
      model_uv_arr=model_uv_arr,/fill_model_vis,timing=model_timing,silent=silent,error=error,/calibration_flag,$
      spectral_model_uv_arr=spectral_model_uv_arr,_Extra=extra)
  endif else begin
    ;; Option to transfer pre-made and unflagged model visbilities
    vis_model_arr=PTRARR(obs.n_pol,/allocate)

    for pol_i=0, obs.n_pol-1 do begin
      transfer_name = model_transfer + '/' + obs.obsname + '_vis_model_'+obs.pol_names[pol_i]+'.sav'
      if ~file_test(transfer_name) then $
        message, transfer_name + ' not found during model transfer.'
      vis_model_arr[pol_i] = getvar_savefile(transfer_name,'vis_model_ptr')
    endfor
  endelse
  t1=Systime(1)-t0_0

  ;; Option to save unflagged model visibilities as part of a calibration-only loop.
  if keyword_set(cal_stop) then begin
    name_cal_stop = file_dirname(file_path_fhd) + '/cal_prerun/' + file_basename(file_path_fhd)
    vis_export,obs,status_str,vis_model_arr,file_path_fhd=name_cal_stop ,/compress,/model
    save, model_uv_arr, filename = name_cal_stop + '_model_uv_arr.sav'
  endif

  ;; Calculate auto-correlation visibilities, optionally use them for initial calibration estimates
  vis_auto=vis_extract_autocorr(obs,vis_arr = vis_ptr,/time_average,auto_tile_i=auto_tile_i)
  IF Keyword_Set(cal.auto_initialize) THEN BEGIN
    IF Keyword_Set(vis_auto) THEN $
      initial_calibration=vis_cal_auto_init(obs,psf,cal,vis_arr=vis_ptr,vis_model_arr=vis_model_arr,_Extra=extra) $
    ELSE print,"calibration_auto_initialize is set, but autocorrelation visibilities are missing. Skipping."
  ENDIF
  vis_auto_model=vis_extract_autocorr(obs,vis_arr = vis_model_arr,/time_average,auto_tile_i=auto_tile_i)

  ;; Set initial calibration given filepath to a cal sav file, the actual cal structure, or the cal gain pointer
  CASE size(initial_calibration,/type) OF
    0:;do nothing if undefined
    7:BEGIN ;type code 7 is string
      file_path_use=initial_calibration
      IF StrLowCase(Strmid(file_path_use,2,3,/reverse_offset)) NE 'sav' THEN file_path_use+='.sav'
      IF file_test(file_path_use) EQ 0 THEN file_path_use=filepath(file_path_use,root=file_dirname(file_path_fhd))
      IF file_test(file_path_use) THEN BEGIN
        cal_init=getvar_savefile(file_path_use,'cal')
        cal.gain=cal_init.gain
        print,'Using initial calibration solution from '+initial_calibration
      ENDIF else print, 'Initial calibration file not found'
    END
    8:cal.gain=initial_calibration.gain ;type code 8 is structure
    10:cal.gain=initial_calibration ;type code 10 is pointer
    ;if set to a numeric type, assume this calibration solution will be wanted for future iterations
    ELSE:IF Keyword_Set(initial_calibration) THEN initial_calibration=file_path_fhd+'_cal'
  ENDCASE

  IF Keyword_Set(error) THEN BEGIN
    timing=Systime(1)-t0_0
    RETURN,vis_ptr
  ENDIF

  ;; extract information from the structures
  pol_names=obs.pol_names
  n_pol=obs.n_pol
  nc_pol=cal.n_pol
  n_freq=cal.n_freq
  n_tile=cal.n_tile
  n_time=cal.n_time

  bandpass_calibrate=cal.bandpass
  calibration_polyfit=cal.polyfit

  tile_A_i=cal.tile_A-1
  tile_B_i=cal.tile_B-1
  freq_arr=cal.freq
  bin_offset=cal.bin_offset
  n_baselines=obs.nbaselines
  tile_A_i=tile_A_i[0:n_baselines-1]
  tile_B_i=tile_B_i[0:n_baselines-1]

  IF N_Elements(vis_weight_ptr) EQ 0 THEN BEGIN
    flag_init=Replicate(1.,n_freq,n_baselines*Float(n_time))
    vis_weight_ptr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *vis_weight_ptr[pol_i]=flag_init
  ENDIF

  ;; calibration loop
  IF N_Elements(preserve_visibilities) EQ 0 THEN preserve_visibilities=0
  IF Keyword_Set(calibration_visibilities_subtract) OR Keyword_Set(vis_baseline_hist) $
    OR Keyword_Set(return_cal_visibilities) THEN preserve_visibilities=1
  IF N_Elements(calibration_flag_iterate) EQ 0 THEN calibration_flag_iterate=0

  t2=0
  FOR iter=0,calibration_flag_iterate DO BEGIN
    t2_a=Systime(1)
    IF iter LT calibration_flag_iterate THEN preserve_flag=1 ELSE preserve_flag=preserve_visibilities
    cal=vis_calibrate_subroutine(vis_ptr,vis_model_arr,vis_weight_ptr,obs,cal,$
      preserve_visibilities=preserve_flag,_Extra=extra)
    IF Keyword_Set(flag_calibration) THEN vis_calibration_flag,obs,cal,n_tile_cut=n_tile_cut,_Extra=extra
    IF Keyword_Set(n_tile_cut) THEN BREAK
  ENDFOR
  t3_a=Systime(1)
  t2+=t3_a-t2_a
  cal_base=Pointer_copy(cal)

  ;; Perform bandpass (amp+phase per fine freq) and polynomial fitting (low order amp+phase fit plus cable reflection fit)
  IF Keyword_Set(bandpass_calibrate) THEN BEGIN
    IF Keyword_Set(auto_ratio_calibration) THEN BEGIN
      cal=cal_auto_ratio(obs,cal,auto_ratio=auto_ratio,vis_auto=vis_auto,auto_tile_i=auto_tile_i,/divide)
    ENDIF

    cal_bandpass=vis_cal_bandpass(cal,obs,params,cal_remainder=cal_remainder,auto_ratio_calibration=auto_ratio_calibration,$
      file_path_fhd=file_path_fhd,_Extra=extra)
 
    IF Keyword_Set(calibration_polyfit) THEN BEGIN
      cal_polyfit=vis_cal_polyfit(cal_remainder,obs,auto_ratio=auto_ratio,_Extra=extra)

      cal=vis_cal_combine(cal_polyfit,cal_bandpass)
    ENDIF ELSE cal=cal_bandpass
    
    IF Keyword_Set(auto_ratio_calibration) THEN BEGIN
      cal=cal_auto_ratio(obs,cal,auto_ratio=auto_ratio,auto_tile_i=auto_tile_i,/remultiply)    
    ENDIF
  ENDIF ELSE IF Keyword_Set(calibration_polyfit) THEN cal=vis_cal_polyfit(cal,obs,_Extra=extra)
   
  ;; In-situ simulation -- forced calibration solutions
  if keyword_set(sim_over_calibrate) then begin
    *cal.gain[0] = (*cal_base.gain[0])
    *cal.gain[1] = (*cal_base.gain[1])
    print, "Forcing per-frequency solutions on the simulated gains"
  endif
  if keyword_set(sim_perf_calibrate) then begin
    (*cal.gain[0])[*,*] = 1.
    (*cal.gain[1])[*,*] = 1.
    print, "Forcing perfect solutions on the simulated gains"
  endif
    
  ;; Get amp from auto-correlation visibilities for plotting (or optionally for the calibration solution itself)
  IF Keyword_Set(vis_auto_model) THEN BEGIN
    cal_auto=vis_cal_auto_fit(obs,cal,vis_auto=vis_auto,vis_model_auto=vis_auto_model,auto_tile_i=auto_tile_i)
  ENDIF
  IF Keyword_Set(calibration_auto_fit) THEN BEGIN
    cal_res=vis_cal_subtract(cal_base,cal_auto)
  ENDIF ELSE cal_res=vis_cal_subtract(cal_base,cal)
  
  basename=file_basename(file_path_fhd)
  dirpath=file_dirname(file_path_fhd)
  image_path=filepath(basename,root=dirpath,sub='output_images')
  IF ~Keyword_Set(no_png) THEN BEGIN
      ;; make sure to plot both, if autocorrelations are used for the calibration solution
      plot_cals,cal,obs,cal_res=cal_res,cal_auto=cal_auto,file_path_base=image_path,_Extra=extra
  ENDIF

  IF Keyword_Set(calibration_auto_fit) THEN cal=cal_auto
  vis_cal=vis_calibration_apply(vis_ptr,cal, vis_model_ptr=vis_model_arr, vis_weight_ptr=vis_weight_ptr)
  cal.gain_residual=cal_res.gain
  undefine_fhd,cal_base

  IF Keyword_Set(vis_baseline_hist) THEN BEGIN
    vis_baseline_hist,obs,params,vis_arr=vis_cal,vis_model_arr=vis_model_arr,file_path_fhd=file_path_fhd
  ENDIF  

  IF ~Keyword_Set(return_cal_visibilities) THEN preserve_visibilities=0
  IF Keyword_Set(calibration_visibilities_subtract) THEN BEGIN
    FOR pol_i=0,n_pol-1 DO *vis_cal[pol_i]-=*vis_model_arr[pol_i]
    IF tag_exist(obs,'residual') THEN obs.residual=1
  ENDIF
  IF ~Keyword_Set(return_cal_visibilities) THEN undefine_fhd,vis_model_arr

  ;; Statistics for metadata reporting
  cal_gain_avg=Fltarr(nc_pol)
  cal_res_avg=Fltarr(nc_pol)
  cal_res_restrict=Fltarr(nc_pol)
  cal_res_stddev=Fltarr(nc_pol)
  FOR pol_i=0,nc_pol-1 DO BEGIN
    tile_use_i=where((*obs.baseline_info).tile_use,n_tile_use)
    freq_use_i=where((*obs.baseline_info).freq_use,n_freq_use)
    IF n_tile_use EQ 0 OR n_freq_use EQ 0 THEN CONTINUE
    gain_ref=extract_subarray(*cal.gain[pol_i],freq_use_i,tile_use_i)
    gain_res=extract_subarray(*cal_res.gain[pol_i],freq_use_i,tile_use_i)
    cal_gain_avg[pol_i]=Mean(Abs(gain_ref))
    cal_res_avg[pol_i]=Mean(Abs(gain_res))
    resistant_mean,Abs(gain_res),2,res_mean
    cal_res_restrict[pol_i]=res_mean
    cal_res_stddev[pol_i]=Stddev(Abs(gain_res))
  ENDFOR
  IF Tag_exist(cal,'Mean_gain') THEN cal.mean_gain=cal_gain_avg
  IF Tag_exist(cal,'Mean_gain_residual') THEN cal.mean_gain_residual=cal_res_avg
  IF Tag_exist(cal,'Mean_gain_restrict') THEN cal.mean_gain_restrict=cal_res_restrict
  IF Tag_exist(cal,'Stddev_gain_residual') THEN cal.stddev_gain_residual=cal_res_stddev

  t3=Systime(1)-t3_a
  timing=Systime(1)-t0_0
  IF ~Keyword_Set(silent) THEN print,timing,t1,t2,t3
  RETURN,vis_cal
END
