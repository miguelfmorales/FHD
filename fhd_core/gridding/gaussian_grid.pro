FUNCTION gaussian_grid,vis_arr_use,obs,psf,params,xcen=xcen,ycen=ycen,vis_inds_use=vis_inds_use,xmin=xmin,$
  model_use=model_use,model_return=model_return,weights=weights,variance=variance,polarization=polarization,$
  fi_use=fi_use,n_vis=n_vis,vis_weights=vis_weights,_Extra=extra

  heap_gc
  i = Complex(0,1)

  ;extract information from the structures
  p=*psf.beam_gaussian_params[polarization]
  var = reform(p,5,N_elements(p)/5)
  dimension=Long(obs.dimension)
  elements=Long(obs.elements)

  frequency_array=((*obs.baseline_info).freq)[fi_use]
  n_f_use=N_Elements(fi_use)

  kx = ((FINDGEN(dimension)-dimension/2.)#(FLTARR(elements)+1.))
  kx_vec = reform(temporary(kx),dimension*elements)
  ky = ((FLTARR(dimension)+1.)#(FINDGEN(elements)-elements/2.))
  ky_vec = reform(temporary(ky),dimension*elements)

  weights_vec=complex(FLTARR(dimension*elements))
  variance_vec=complex(FLTARR(dimension*elements))
  model_vec=complex(FLTARR(dimension*elements))
  data_vec=complex(FLTARR(dimension*elements))

  ;Define frequently used constants outside loops for speed
  normi = 2.*!dpi/(dimension*elements)
  amp_norm = float(reform(var[0,*]*2.*!dpi/(dimension*elements)*var[2,*]*var[4,*]))
  norm2 = 2*!dpi^2/(dimension*elements)

  ;Reform gaussian variables as vectors for speedy matrix multiplications  
  var2 = float(reform(var[2,*])*sqrt(norm2))
  var4 = float(reform(var[4,*])*sqrt(norm2))
  var1 = float(reform(var[1,*])*normi)
  var3 = float(reform(var[3,*])*normi)

  ;;Loop through each baseline location for gridding
  ;; Baselines do not move with time, so loop over unique baseline locations for one time sample
  for freq_i=0, n_f_use-1 do begin

    ;Only grid unflagged baselines
    inds = where(xmin[freq_i,*] GT 0,ncount)
    if ncount EQ 0 then continue

    ;Capture the baseline location inds from the *last* time sample (default operation of uniq)
    ;uniq_time_inds = uniq(params.time[reform(vis_inds_use[freq_i,inds])])
    ;n_uniq_baselines = uniq_time_inds[0]
    ;similar_baselines = reform(inds[[0,uniq_time_inds]])
    uniq_time_inds = uniq(params.time[reform(vis_inds_use[freq_i,inds])])
    select_baselines = reform(params.baseline_arr[vis_inds_use[freq_i,inds]])
    temp = uniq(select_baselines,sort(select_baselines)) 
    n_uniq_baselines = N_elements(temp)
    similar_baselines = uniq_time_inds
    n_similar = N_elements(similar_baselines)
    x_base_i = float(xcen[freq_i,inds[temp]])
    y_base_i = float(ycen[freq_i,inds[temp]])

    ;Reduce array outside of baseline for loop for speed
    vis_arr_use_ref = reform(vis_arr_use[freq_i,inds])
    model_use_ref = reform(model_use[freq_i,inds])

    for base_i=0,n_uniq_baselines-1 do begin

      ;Calculate the kx,ky grid. Offset the grid to match baseline location 
      kx_base_vec = kx_vec-x_base_i[base_i]
      ky_base_vec = ky_vec-y_base_i[base_i]

      ;Calculate terms individually to utilize cores as efficiently as possible
      phase_term = matrix_multiply(var1,kx_base_vec)+matrix_multiply(var3,ky_base_vec)
      cos_term=cos(phase_term)
      sin_term=sin(phase_term)
      phase_term = Complex(cos_term,-sin_term)
      gauss_term = -((matrix_multiply(var2,kx_base_vec))^2+(matrix_multiply(var4,ky_base_vec))^2)
      exp_term = exp(gauss_term)

      beam_vec = conj(reform(matrix_multiply(amp_norm, exp_term*phase_term)))
      
      temp1 = where(select_baselines EQ select_baselines[temp[base_i]],n_count) 

      weights_vec += beam_vec*n_count
      variance_vec += abs(beam_vec)^2.*n_count
      data_vec += total(vis_arr_use_ref[temp1])*beam_vec
      model_vec += total(model_use_ref[temp1])*beam_vec

    endfor

  endfor

  image_uv = reform((data_vec),dimension,elements)/n_vis
  model_return = reform((model_vec),dimension,elements)/n_vis
  weights = reform((weights_vec),dimension,elements)/n_vis
  variance = reform((variance_vec),dimension,elements)/n_vis

  return, image_uv

end
