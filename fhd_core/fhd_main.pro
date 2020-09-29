PRO fhd_main, file_path_vis, status_str, export_images=export_images, cleanup=cleanup, recalculate_all=recalculate_all,$
    mapfn_recalculate=mapfn_recalculate, grid_recalculate=grid_recalculate,beam_recalculate=beam_recalculate,$
    n_pol=n_pol, flag_visibilities=flag_visibilities, silent=silent, deconvolve=deconvolve, transfer_mapfn=transfer_mapfn,$
    tile_flag_list=tile_flag_list, diffuse_calibrate=diffuse_calibrate, diffuse_model=diffuse_model,$
    file_path_fhd=file_path_fhd, force_data=force_data, force_no_data=force_no_data, freq_start=freq_start, freq_end=freq_end,$
    calibrate_visibilities=calibrate_visibilities, transfer_calibration=transfer_calibration, error=error,$
    calibration_catalog_file_path=calibration_catalog_file_path, dft_threshold=dft_threshold,$
    calibration_visibilities_subtract=calibration_visibilities_subtract,$
    weights_grid=weights_grid, save_visibilities=save_visibilities, return_cal_visibilities=return_cal_visibilities,$
    return_decon_visibilities=return_decon_visibilities, snapshot_healpix_export=snapshot_healpix_export, cmd_args=cmd_args, log_store=log_store,$
    generate_vis_savefile=generate_vis_savefile, model_visibilities=model_visibilities, model_catalog_file_path=model_catalog_file_path,$
    transfer_weights=transfer_weights, flag_calibration=flag_calibration, production=production, deproject_w_term=deproject_w_term, $
    in_situ_sim_input=in_situ_sim_input, eor_vis_filepath=eor_vis_filepath, enhance_eor=enhance_eor, sim_noise=sim_noise,cal_stop=cal_stop, $
    remove_sim_flags=remove_sim_flags,model_uv_transfer=model_uv_transfer,extra_vis_filepath=extra_vis_filepath,_Extra=extra

compile_opt idl2,strictarrsubs    
except=!except
!except=0 ;System variable that controls when math errors are printed. Set to 0 to disable.
error=0
heap_gc 
t0=Systime(1)
start_mem = memory(/current)

print,"Processing "+file_basename(file_path_vis)+" in "+file_dirname(file_path_vis)
print,systime()
print,'Output file_path:',file_path_fhd

log_filepath=file_path_fhd+'_log.txt'
IF Keyword_Set(!Journal) THEN journal

data_flag=fhd_setup(file_path_vis,status_str,export_images=export_images,cleanup=cleanup,recalculate_all=recalculate_all,$
    mapfn_recalculate=mapfn_recalculate,grid_recalculate=grid_recalculate,beam_recalculate=beam_recalculate,$
    n_pol=n_pol,flag_visibilities=flag_visibilities,deconvolve=deconvolve,transfer_mapfn=transfer_mapfn,$
    transfer_weights=transfer_weights,file_path_fhd=file_path_fhd,force_data=force_data,force_no_data=force_no_data,$
    calibrate_visibilities=calibrate_visibilities,transfer_calibration=transfer_calibration,$
    weights_grid=weights_grid,save_visibilities=save_visibilities,$
    snapshot_healpix_export=snapshot_healpix_export,log_store=log_store,_Extra=extra)
    
IF data_flag LE 0 THEN BEGIN
  
    IF Keyword_Set(log_store) THEN Journal,log_filepath
    fhd_save_io,status_str,file_path_fhd=file_path_fhd,/reset
    
    uvfits_read,hdr,params,layout,vis_arr,vis_weights,file_path_vis=file_path_vis,n_pol=n_pol,silent=silent,error=error,_Extra=extra
    IF Keyword_Set(error) THEN BEGIN
      print,"Error occured while reading uvfits data. Returning."
      RETURN
    ENDIF
    fhd_save_io,status_str,layout,var='layout',/compress,file_path_fhd=file_path_fhd,_Extra=extra ;save layout structure right away for debugging. Will be overwritten a few times before the end
    
    ;In situ simulations given input model visibilities as dirty visilibilities
    If keyword_set(in_situ_sim_input) then $
        in_situ_sim_setup, in_situ_sim_input, vis_arr, vis_weights, flag_calibration,n_pol=n_pol,enhance_eor=enhance_eor, $
          eor_vis_filepath=eor_vis_filepath,file_path_vis=file_path_vis,file_path_fhd=file_path_fhd,sim_noise=sim_noise, $
          hdr=hdr,params=params,calibration_catalog_file_path=calibration_catalog_file_path, $
          diffuse_calibrate=diffuse_calibrate,transfer_calibration=transfer_calibration,freq_start=freq_start,$
          freq_end=freq_end,tile_flag_list=tile_flag_list,deproject_w_term=deproject_w_term,dft_threshold=dft_threshold,$
          remove_sim_flags=remove_sim_flags,extra_vis_filepath=extra_vis_filepath,use_extra_flags=use_extra_flags,_Extra=extra
    
    IF Keyword_Set(generate_vis_savefile) THEN BEGIN
        IF Strpos(file_path_vis,'.sav') EQ -1 THEN file_path_vis_sav=file_path_vis+".sav" ELSE file_path_vis_sav=file_path_vis
        SAVE,vis_arr,vis_weights,hdr,params,filename=file_path_vis_sav
        timing=Systime(1)-t0
        IF ~Keyword_Set(silent) THEN print,'Processing time (minutes): ',Strn(Round(timing/60.))
        RETURN
    ENDIF
    
    ;Note: explicitly reference dft_threshold here to remove it from EXTRA, which would be passed on to lower-level routines
    obs=fhd_struct_init_obs(file_path_vis,hdr,params,n_pol=n_pol,dft_threshold=dft_threshold,_Extra=extra) 
    n_pol=obs.n_pol
    n_freq=obs.n_freq
    
    fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra ;save obs structure right away for debugging. Will be overwritten a few times before the end
    IF Keyword_Set(deproject_w_term) THEN vis_arr=simple_deproject_w_term(obs,params,vis_arr,direction=deproject_w_term)
    
    ;Construct a new beam model. Also sets up the structure PSF
    print,'Calculating beam model'
    psf=beam_setup(obs,status_str,antenna,file_path_fhd=file_path_fhd,restore_last=0,silent=silent,timing=t_beam,no_save=no_save,_Extra=extra)
    IF Keyword_Set(t_beam) THEN IF ~Keyword_Set(silent) THEN print,'Beam modeling time: ',t_beam
    jones=fhd_struct_init_jones(obs,status_str,file_path_fhd=file_path_fhd,restore=0,mask=beam_mask,_Extra=extra)

    IF Keyword_Set(transfer_weights) THEN BEGIN
        flag_visibilities=0 ;
        transfer_weights_data,vis_weights,obs,status_str,file_path_fhd=file_path_fhd,$
            transfer_filename=transfer_weights,error=error,flag_visibilities=flag_visibilities,$
            flag_calibration=flag_calibration,_Extra=extra
        IF Keyword_Set(error) THEN BEGIN
            print,"Error occured while attempting to transfer weights. Returning."
            RETURN
        ENDIF

    ENDIF
    
    ;Bypass main flagging for in situ simulations
    If ~keyword_set(remove_sim_flags) then $
        vis_weights=vis_flag_basic(vis_weights,obs,params,n_pol=n_pol,n_freq=n_freq,freq_start=freq_start,$
           freq_end=freq_end,tile_flag_list=tile_flag_list,vis_ptr=vis_arr,_Extra=extra)
    vis_weights_update,vis_weights,obs,psf,params,_Extra=extra
    
    IF Keyword_Set(calibrate_visibilities) THEN BEGIN
      IF Keyword_Set(calibration_catalog_file_path) THEN catalog_use=calibration_catalog_file_path
      IF ~Keyword_set(transfer_calibration) AND ~Keyword_set(model_uv_transfer) THEN BEGIN
        IF ~Keyword_Set(calibration_source_list) THEN $
          calibration_source_list=generate_source_cal_list(obs,psf,catalog_path=catalog_use,_Extra=extra)
      ENDIF
      skymodel_cal=fhd_struct_init_skymodel(obs,source_list=calibration_source_list,catalog_path=catalog_use,return_cal=1,diffuse_model=diffuse_calibrate,_Extra=extra)
      cal=fhd_struct_init_cal(obs,params,skymodel_cal,source_list=calibration_source_list,$
        catalog_path=catalog_use,transfer_calibration=transfer_calibration,_Extra=extra)
    ENDIF

    ;print informational messages
    obs_status,obs
    fhd_log_settings,file_path_fhd,obs=obs,psf=psf,cal=cal,layout=layout,antenna=antenna,cmd_args=cmd_args,/overwrite,sub_dir='metadata'  ;write preliminary settings file for debugging, in case later steps crash
    IF obs.n_tile_flag EQ obs.n_tile THEN BEGIN
        ; Exit if all tiles are flagged.
        ; This is checked after optionally transfering or modifying the weights, and after writing the settings file.
        error=1
        print,"All tiles flagged! No data left to use. Returning"
        RETURN
    ENDIF
    
    IF Keyword_Set(transfer_calibration) THEN BEGIN
      calibrate_visibilities=1
      IF size(transfer_calibration,/type) LT 7 THEN transfer_calibration=file_path_fhd ;this will not modify string or structure types, but will over-write it if set to a numerical type
    ENDIF

    IF Keyword_set(model_uv_transfer) THEN BEGIN
      if ~file_test(model_uv_transfer) then message, model_uv_transfer + ' not found during model uv transfer.'
      model_uv_arr = getvar_savefile(model_uv_transfer,'model_uv_arr')
      IF Keyword_Set(model_visibilities) THEN model_uv_arr2 = model_uv_arr
    ENDIF

    IF Keyword_Set(calibrate_visibilities) THEN BEGIN
        print,"Calibrating visibilities"
        vis_arr=vis_calibrate(vis_arr,cal,obs,status_str,psf,params,jones,$
            vis_weight_ptr=vis_weights,file_path_fhd=file_path_fhd,$
             transfer_calibration=transfer_calibration,timing=cal_timing,error=error,model_uv_arr=model_uv_arr,$
             return_cal_visibilities=return_cal_visibilities,vis_model_arr=vis_model_arr,$
             calibration_visibilities_subtract=calibration_visibilities_subtract,silent=silent,$
             flag_calibration=flag_calibration,cal_stop=cal_stop,_Extra=extra)
        IF Keyword_Set(return_cal_visibilities) THEN $
            vis_calibrate_qu_mixing,vis_arr,vis_model_arr,vis_weights,obs,cal
        IF ~Keyword_Set(silent) THEN print,String(format='("Calibration timing: ",A)',Strn(cal_timing))
        IF Keyword_Set(error) THEN BEGIN
            print,"Error occured during calibration. Returning."
            RETURN
        ENDIF
        fhd_save_io,status_str,cal,var='cal',/compress,file_path_fhd=file_path_fhd,_Extra=extra
        vis_weights_update,vis_weights,obs,psf,params,_Extra=extra
        if keyword_set(cal_stop) then begin
          fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra ;need beam_integral for PS
          print, "cal_stop initiated. Returning."
          run_report, start_mem, t0, silent=silent
          RETURN
        endif
    ENDIF
    
    IF Keyword_Set(flag_visibilities) THEN BEGIN
        print,'Flagging anomalous data'
        vis_flag,vis_arr,vis_weights,obs,psf,params,_Extra=extra
        fhd_save_io,status_str,vis_weights,var='vis_weights',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    ENDIF ELSE $ ;saved weights are needed for some later routines, so save them even if no additional flagging is done
        fhd_save_io,status_str,vis_weights,var='vis_weights',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    
    IF Keyword_Set(model_visibilities) THEN BEGIN
        IF Keyword_Set(model_catalog_file_path) AND ~Keyword_set(model_uv_transfer) THEN BEGIN
            model_source_list=generate_source_cal_list(obs,psf,catalog_path=model_catalog_file_path,/model_visibilities,_Extra=extra) 
            IF Keyword_Set(return_cal_visibilities) OR Keyword_Set(calibration_visibilities_subtract) THEN $
                model_source_list=source_list_append(obs,model_source_list,skymodel_cal.source_list,/exclude)
        ENDIF
        skymodel_update=fhd_struct_init_skymodel(obs,source_list=model_source_list,catalog_path=model_catalog_file_path,$
            diffuse_model=diffuse_model,return_cal=return_cal_visibilities,_Extra=extra)
        vis_model_arr=vis_source_model(skymodel_update,obs,status_str,psf,params,vis_weights,0,jones,model_uv_arr=model_uv_arr2,$
            timing=model_timing,silent=silent,error=error,vis_model_ptr=vis_model_arr,calibration_flag=0,_Extra=extra) 
        IF Min(Ptr_valid(model_uv_arr)) GT 0 THEN FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*model_uv_arr2[pol_i] $
            ELSE model_uv_arr=Pointer_copy(model_uv_arr2) 
        undefine_fhd,model_uv_arr2
    ENDIF
    
    IF Keyword_Set(skymodel_cal) OR Keyword_Set(skymodel_update) THEN $
      skymodel=fhd_struct_combine_skymodel(obs, skymodel_cal, skymodel_update,calibration_flag=(Keyword_Set(return_cal_visibilities) OR Keyword_Set(calibration_visibilities_subtract)))
    fhd_save_io,status_str,skymodel,var='skymodel',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    
    IF N_Elements(vis_model_arr) LT n_pol THEN vis_model_arr=Ptrarr(n_pol) ;supply as array of null pointers to allow it to be indexed, but not be used
    model_flag=min(Ptr_valid(vis_model_arr))
    
    IF Keyword_Set(error) THEN BEGIN
      print,"Error occured during source modeling"
      RETURN
    ENDIF
    
    IF N_Elements(source_array) GT 0 THEN BEGIN
        fhd_save_io,status_str,source_array,var='source_array',/compress,file_path_fhd=file_path_fhd,path_use=path_use,_Extra=extra
        source_array_export,source_array,obs,file_path=path_use
    ENDIF
    
    vis_noise_calc,obs,vis_arr,vis_weights
    IF ~Keyword_Set(silent) THEN flag_status,obs
    fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,params,var='params',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,layout,var='layout',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_log_settings,file_path_fhd,obs=obs,layout=layout,psf=psf,cal=cal,antenna=antenna,skymodel=skymodel,cmd_args=cmd_args,/overwrite,sub_dir='metadata'
    undefine_fhd,antenna
    auto_corr=vis_extract_autocorr(obs,status_str,vis_arr=vis_arr,file_path_fhd=file_path_fhd,_Extra=extra)
    
    IF obs.n_vis EQ 0 THEN BEGIN
        print,"All data flagged! Returning."
        error=1
        IF Keyword_Set(!Journal) THEN Journal ;write and close log file if present
        RETURN
    ENDIF
    
    IF Keyword_Set(return_decon_visibilities) THEN save_visibilities=1
    IF Keyword_Set(save_visibilities) THEN BEGIN
        t_save0=Systime(1)
        vis_export,obs,status_str,vis_arr,vis_weights,file_path_fhd=file_path_fhd,/compress
        IF Keyword_Set(model_flag) THEN vis_export,obs,status_str,vis_model_arr,vis_weights,file_path_fhd=file_path_fhd,/compress,/model
        t_save=Systime(1)-t_save0
        IF ~Keyword_Set(silent) THEN print,'Visibility save time: ',t_save
    ENDIF
    
    ;Grid the visibilities
    IF Keyword_Set(grid_recalculate) THEN BEGIN
        image_uv_arr=visibility_grid_wrap(vis_arr,vis_weights,obs,status_str,psf,params,file_path_fhd=file_path_fhd,vis_model_arr=vis_model_arr,$
            deconvolve=deconvolve,model_flag=model_flag,snapshot_healpix_export=snapshot_healpix_export,mapfn_recalculate=mapfn_recalculate,$
            save_visibilities=save_visibilities,error=error,no_save=no_save,weights_arr=weights_arr,model_uv_holo=model_uv_holo,$
            return_decon_visibilities=return_decon_visibilities,_Extra=extra)
    ENDIF ELSE BEGIN
      print,'Visibilities not re-gridded'
    ENDELSE
    IF ~Keyword_Set(snapshot_healpix_export) THEN Ptr_free,vis_arr,vis_weights,vis_model_arr
    IF Keyword_Set(!Journal) THEN Journal ;write and close log file if present
ENDIF
  
IF N_Elements(cal) EQ 0 THEN fhd_save_io,status_str,cal,var='cal',/restore,file_path_fhd=file_path_fhd,/compatibility_mode,_Extra=extra
IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,/compatibility_mode,_Extra=extra
;deconvolve point sources using fast holographic deconvolution
IF Keyword_Set(deconvolve) THEN BEGIN
    print,'Deconvolving point sources'

    fhd_wrap,obs,status_str,psf,params,fhd_params,cal,jones,skymodel,file_path_fhd=file_path_fhd,silent=silent,$
        transfer_mapfn=transfer_mapfn,map_fn_arr=map_fn_arr,image_uv_arr=image_uv_arr,weights_arr=weights_arr,$
        vis_model_arr=vis_model_arr,return_decon_visibilities=return_decon_visibilities,model_uv_arr=model_uv_arr,$
        log_store=log_store,vis_weights=vis_weights,_Extra=extra
    IF Keyword_Set(return_decon_visibilities) AND Keyword_Set(save_visibilities) THEN vis_export,obs,status_str,vis_model_arr,vis_weights,file_path_fhd=file_path_fhd,/compress,/model
ENDIF ELSE BEGIN
    print,'Gridded visibilities not deconvolved'
ENDELSE

;Generate fits data files and images
IF Keyword_Set(export_images) THEN BEGIN
    fhd_quickview,obs,status_str,psf,cal,jones,skymodel,fhd_params,image_uv_arr=image_uv_arr,weights_arr=weights_arr,$
        model_uv_holo=model_uv_holo,beam_arr=beam_arr,file_path_fhd=file_path_fhd,silent=silent,$
        map_fn_arr=map_fn_arr,transfer_mapfn=transfer_mapfn,_Extra=extra
ENDIF

;optionally export frequency-splt Healpix cubes
IF Keyword_Set(snapshot_healpix_export) THEN healpix_snapshot_cube_generate,obs,status_str,psf,cal,params,vis_arr,$
    vis_model_arr=vis_model_arr,file_path_fhd=file_path_fhd,vis_weights=vis_weights,cmd_args=cmd_args,_Extra=extra

;Optionally fill the fhd table on the mwa_qc database located on eor-00 under the mwa username. See the python script 
;for more information about possible queries.
IF Keyword_Set(production) THEN BEGIN
  ;Set up paths and keywords for the database filler, including the complete tag
  IF ~Keyword_Set(error) THEN complete='1'
  descr_file_path=file_path_fhd+'_settings.txt'
  descr_file_path=filepath(file_basename(descr_file_path),root=file_dirname(descr_file_path),sub="metadata")

  ;Spawn a child process to run the database filler, located in MWA_Tools
  SPAWN, 'fhd_database_filler.py -s ' + descr_file_path + ' -o ' + obs.obsname + ' -c ' + obs.code_version + $
      ' -p ' + file_path_fhd + ' -m ' + complete
ENDIF

undefine_fhd,map_fn_arr,image_uv_arr,weights_arr,model_uv_arr,vis_arr,vis_weights,vis_model_arr
undefine_fhd,obs,cal,jones,layout,psf,antenna,fhd_params,skymodel,skymodel_cal,skymodel_update

run_report, start_mem, t0, silent=silent
print,''
!except=except

END
