PRO in_situ_sim_setup, in_situ_sim_input, vis_arr, vis_weights, flag_calibration, n_pol=n_pol, enhance_eor=enhance_eor, $
    eor_vis_filepath=eor_vis_filepath, file_path_vis=file_path_vis, file_path_fhd=file_path_fhd,sim_noise=sim_noise, $
    hdr=hdr, params=params, calibration_catalog_file_path=calibration_catalog_file_path, $
    diffuse_calibrate=diffuse_calibrate,transfer_calibration=transfer_calibration,freq_start=freq_start, $
    freq_end=freq_end,tile_flag_list=tile_flag_list,deproject_w_term=deproject_w_term,dft_threshold=dft_threshold, $
    remove_sim_flags=remove_sim_flags,extra_vis_filepath=extra_vis_filepath,_Extra=extra
    
  print, "Performing in-situ simulation"
  
  if ~keyword_set(n_pol) then n_pol=2
  if n_pol EQ 2 then pol_name=['XX','YY'] else pol_name=['XX','YY','XY','YX']
  
  vis_model_arr=PTRARR(n_pol,/allocate)
  obs_id = file_basename(file_path_vis, '.uvfits')
  
  ;restore model visibilities given the in_situ_sim_input to act as the input data visibilities
  if typename(in_situ_sim_input) EQ 'STRING' then begin
    for pol_i=0, n_pol-1 do begin
    
      ;if directory, assume we are looking for model vis sav files
      if total(file_test(in_situ_sim_input,/directory)) GT 0 then begin
        file_name = in_situ_sim_input+'/'+obs_id+'_vis_model_'+pol_name[pol_i]+'.sav'
        if total(file_test(file_name)) LT 1 then $
          message, file_name + ' not found'
        vis_model_arr[pol_i] = GETVAR_SAVEFILE(file_name, 'vis_model_ptr')
      ;if a uvfits file, assume it has all the polarizations
      endif else if strmid(in_situ_sim_input,5,6,/reverse_offset) EQ 'uvfits' then begin
        file_name = in_situ_sim_input
        if total(file_test(file_name)) LT 1 then message, file_name + ' not found'
        uvfits_read,hdr,params,layout,vis_model_arr,vis_weights,file_path_vis=file_name,$
          n_pol=n_pol,silent=silent,error=error,_Extra=extra
      endif
      print, "Using " + file_name + ' as input model'
      
    endfor
  endif else message, 'Please specify the directory and/or filename of the input visibilities in in_situ_sim_input'
  
  vis_arr=temporary(vis_model_arr)
  
  ;restore EoR visibilities
  If keyword_set(eor_vis_filepath) then begin
    vis_eor=PTRARR(n_pol,/allocate)

    if file_test(eor_vis_filepath) AND (strmid(eor_vis_filepath,5,6,/reverse_offset) EQ 'uvfits') then begin
      uvfits_read,hdr,params,layout,vis_eor,eor_weights,file_path_vis=eor_vis_filepath,n_pol=n_pol,silent=silent,error=error,_Extra=extra

    ;Check size of visibilties 
    for pol_i=0, n_pol-1 do begin
      if (size(*vis_arr[pol_i]))[1] NE (size(*vis_eor[pol_i]))[1] then message, "EoR in situ visibilities do not match inherent vis size"
      if (size(*vis_arr[pol_i]))[2] NE (size(*vis_eor[pol_i]))[2] then message, "EoR in situ visibilities do not match inherent vis size"
    endfor

    ;*Search for the specified eor savefile
    endif else if (total(file_test(eor_vis_filepath)) GT 0) AND (file_test(eor_vis_filepath,/directory) EQ 0) then begin
      size_savefile=(size(eor_vis_filepath))[1]
      
      void = GETVAR_SAVEFILE(eor_vis_filepath[0], names=names)
      vis_varname = names[where(strmatch(names, '*vis*') EQ 1,n_count)]  ;assumption: visibilities in sav file have "vis" in the name
      
      ;Restore visibilities that are in different polarization save files, or restore the all-pol save file
      if size_savefile EQ n_pol then $
        for pol_i=0,n_pol-1 do vis_eor[pol_i] = GETVAR_SAVEFILE(eor_vis_filepath[pol_i], vis_varname) $
      else vis_eor = GETVAR_SAVEFILE(eor_vis_filepath, vis_varname)
      
    endif else begin
      for pol_i=0,n_pol-1 do begin
        if total(file_test(eor_vis_filepath + '/' + obs_id + '_vis_' + pol_name[pol_i] + '.sav')) GT 0 then begin
          void = GETVAR_SAVEFILE(eor_vis_filepath + '/' + obs_id + '_vis_' + pol_name[pol_i] + '.sav', names=names)
          vis_varname = names[where(strmatch(names, '*vis*') EQ 1,n_count)]  ;assumption: visibilities in sav file have "vis" in the name
          
          ;Restore visibilities that are in the <obsid>_vis_XX/vis_YY format
          vis_eor[pol_i] = GETVAR_SAVEFILE(eor_vis_filepath + '/' + obs_id + '_vis_' + pol_name[pol_i] + '.sav', vis_varname)
        endif else message, "eor_vis_filepath not found! Tried " + eor_vis_filepath + " and " + eor_vis_filepath + '/' + obs_id + "_vis_" $
          + pol_name[pol_i] + ".sav (for all pol)"
      endfor
    endelse
    ;*End of search for the specified eor savefile
    
    ;Boost the eor signal by a specified amount
    If keyword_set(enhance_eor) then begin
      print, "Enhancing input EoR by "+strtrim(enhance_eor,2)+"x"
      for pol_i=0,n_pol-1 do (*vis_eor[pol_i])=(*vis_eor[pol_i])*enhance_eor
    endif
    
    ;Combine the calibrated visibilities in the correct format for the script
    for pol_i=0,n_pol-1 do (*vis_arr[pol_i]) = (*vis_arr[pol_i])+(*vis_eor[pol_i])
    
  endif
  
  ;Optionally add noise to the visibilities (from a save file)
  If keyword_set(sim_noise) then begin
  
    if total(file_test(sim_noise)) GT 0 then begin
      ;Restore or create the noise visibilities
      void = getvar_savefile(sim_noise,names=names)
      if strmatch(names, 'obs', /FOLD_CASE) then begin
        obs_restore = getvar_savefile(sim_noise,strjoin((names[where(strmatch(names, 'obs', /FOLD_CASE) EQ 1)])[0]))
        vis_noise = vis_noise_simulation(vis_arr, obs_id, obs_restore, n_pol=n_pol, file_path_fhd=file_path_fhd)
      endif else vis_noise = getvar_savefile(sim_noise,strjoin(names[0]))
      
    endif else message, 'Need to specify either a filepath to an obs with vis_noise calculation or a noise simulation.'
    
    ;Add the noise to the visibilities, but keeping zeroed visibilities fully zero
    for pol_i=0, n_pol-1 do begin
      nonzero_i = where(real_part(*vis_arr[pol_i]) NE 0)
      (*vis_arr[pol_i])[nonzero_i] = (*vis_arr[pol_i])[nonzero_i]+(*vis_noise[pol_i])[nonzero_i]
    endfor
    
  endif

  ;Optionally add any general visibilities
  if keyword_set(extra_vis_filepath) then begin

    if total(file_test(extra_vis_filepath)) GT 0 then begin
      if strmid(extra_vis_filepath,5,6,/reverse_offset) EQ 'uvfits' then begin
        uvfits_read,hdr_extra,params_extra,layout_extra,vis_extra,vis_extra_weights,file_path_vis=extra_vis_filepath,n_pol=n_pol,$
          silent=silent,error=error,_Extra=extra
      endif else message, "File " + extra_vis_filepath + " needs to be a uvfits."
    endif else message, "File " + extra_vis_filepath + " not found."
    
    ;Check size of visibilties 
    for pol_i=0, n_pol-1 do begin
      if (size(*vis_arr[pol_i]))[1] NE (size(*vis_extra[pol_i]))[1] then message, "Extra in situ visibilities do not match inherent vis size"
      if (size(*vis_arr[pol_i]))[2] NE (size(*vis_extra[pol_i]))[2] then message, "Extra in situ visibilities do not match inherent vis size"
    endfor
    
    ;Add visibilities
    for pol_i=0, n_pol-1 do (*vis_arr[pol_i]) = (*vis_arr[pol_i])+(*vis_extra[pol_i])

  endif

  ;Remove all weighting to remove pfb effects and flagged channels. Remove calibration flagging.
  if keyword_set(remove_sim_flags) then for pol_i=0, n_pol-1 do (*vis_weights[pol_i])[*,*]=1.
  flag_calibration=0

  if keyword_set(use_extra_flags) then vis_weights=vis_extra_weights

  return
END
