PRO vis_calibrate_crosspol_phase,vis_ptr,vis_model_ptr,vis_weight_ptr,cal
; Calculates the phase fit between X and Y antenna polarizations
; See crosspol_phase_calculation_memo.pdf for a discussion of this calculation

n_pol = N_Elements(vis_ptr)

IF n_pol EQ 4 THEN BEGIN
  icomp = Complex(0,1)
  n_freq=cal.n_freq
  n_tile=cal.n_tile
  n_time=cal.n_time
  n_baselines=Long(N_Elements(cal.tile_A)/n_time)
  
  ;Use the xx flags (yy should be identical at this point)
  weights_use = 0>Reform(*vis_weight_ptr[0],n_freq,n_baselines,n_time)<1
  
  ;average the visibilities in time
  vis_xy = Reform(*vis_ptr[2],n_freq,n_baselines,n_time)
  vis_xy = Total(Temporary(vis_xy)*weights_use,3)
  vis_yx = Reform(*vis_ptr[3],n_freq,n_baselines,n_time)
  vis_yx = Total(Temporary(vis_yx)*weights_use,3)
  model_xy = Reform(*vis_model_ptr[2],n_freq,n_baselines,n_time)
  model_xy = Total(Temporary(model_xy)*weights_use,3)
  model_yx = Reform(*vis_model_ptr[3],n_freq,n_baselines,n_time)
  model_yx = Total(Temporary(model_yx)*weights_use,3)
  
  ;remove zeros
  weight = Total(Temporary(weights_use),3)
  i_use = Where(weight,n_use)
  vis_xy = Reform(vis_xy[i_use],1,n_use)
  vis_yx = Reform(vis_yx[i_use],1,n_use)
  model_xy = Reform(model_xy[i_use],1,n_use)
  model_yx = Reform(model_yx[i_use],1,n_use)
  
  vis_sum = Total(Conj(Temporary(vis_xy))*Temporary(model_xy)) + Total(Temporary(vis_yx)*Conj(Temporary(model_yx)))
  cross_phase = atan(vis_sum, /phase)
  
  print,"Phase fit between X and Y antenna polarizations:", cross_phase
  
  cal.cross_phase = cross_phase
ENDIF

END
