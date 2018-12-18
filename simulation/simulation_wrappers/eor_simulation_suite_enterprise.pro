pro eor_simulation_suite_enterprise, uvf_input = uvf_input, $
    input_uvfits_file=input_uvfits_file, input_data_directory=input_data_directory, $
    file_uvw = file_uvw, large_healpix = large_healpix, $
    flat_sigma = flat_sigma, run_name = run_name, $
    sample_inds = sample_inds, eor_real_sky = eor_real_sky, $
    recalc_sim = recalc_sim, refresh_ps = refresh_ps, refresh_binning = refresh_binning, $
    no_ps_plots = no_ps_plots, no_suite_plots = no_suite_plots, plot_ratio = plot_ratio, $
    use_peak = use_peak, png = png, eps = eps, pdf = pdf

  if n_elements(flat_sigma) eq 0 then flat_sigma = 1
  if keyword_set(flat_sigma) then sim_in = 'flat' else sim_in = 'eor'

  if keyword_set(large_healpix) then begin
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
  endif
  
  if n_elements(input_uvfits_file) eq 1 then begin
    file_uvw=1
    start_file_index=0
    end_file_index=0
  endif else begin
    start_file_index=36
    end_file_index=36
  endelse 

  if keyword_set(file_uvw) then begin
    if n_elements(run_name) eq 0 and n_elements(input_uvfits_file) eq 0 then run_name = 'mwa_zenith'

    version = 'bjh_sim_' + sim_in + '_' + run_name

    n_samples = 1s
    no_suite_plots = 1
    use_weight_cutoff_sim = 1
  endif else begin
    ;; larger numbers here correspond to increasing baseline density.
    ;; higher densities take more memory. Ideally going up to 5 would be good,
    ;; enterprise doesn't seem to be able to do more than 0.5 without crashing.
    sample_factors = [.0001, .0005, .001, .005, .01, .02, .03, .04, .05, .1, .5, 1]
    ;;sample_factors = [.02, .03, .04, .05, .1, .5, 1]

    if n_elements(run_name) eq 0 then message, 'run name must be specified.'

    if run_name eq 'noflag' then sample_factors = [sample_factors, 5]

    if n_elements(sample_inds) gt 0 then sample_factors = sample_factors[sample_inds]

    version = 'bjh_arrsim_' + sim_in + '_' + run_name + '_density' + number_formatter(sample_factors)

    if n_elements(sample_inds) gt 0 then sample_factors = sample_factors[sample_inds]

    n_samples = n_elements(sample_factors)
    use_weight_cutoff_sim = 0

  endelse
  folder_names = 'fhd_' + version
  folder_path = '/data3/users/bryna/fhd_sims/'

  if keyword_set(png) or keyword_set(eps) or keyword_set(pdf) then pub = 1 else pub = 0
  if pub eq 1 then begin
    if not (keyword_set(png) or keyword_set(eps) or keyword_set(pdf)) then begin
      basename = cgRootName(plotfile, directory=directory, extension=extension)

      case extension of
        'eps': eps=1
        'png': png=1
        'pdf': pdf=1
        '': png = 1
        else: begin
          print, 'Unrecognized extension, using png'
          png = 1
        end
      endcase

    endif

    if keyword_set(png) and keyword_set(eps) and keyword_set(pdf) then begin
      print, 'only one of eps, pdf and png can be set, using png'
      eps = 0
    endif

    if keyword_set(png) then begin
      plot_exten = '.png'
      delete_ps = 1
    endif else if keyword_set(pdf) then begin
      plot_exten = '.pdf'
      delete_ps = 1
    endif else if keyword_set(eps) then begin
      plot_exten = '.eps'
      delete_ps = 0
    endif
  endif

  if n_elements(window_num) eq 0 then window_num = 2

  nbeams=1

  sim_powers = fltarr(nbeams, n_samples)
  sim_ave_powers = fltarr(nbeams, n_samples)
  sim_wt_ave_powers = fltarr(nbeams, n_samples)
  sim_ave_powers_uvf = fltarr(nbeams, n_samples)
  sim_wt_ave_powers_uvf = fltarr(nbeams, n_samples)
  sim_ave_weights = fltarr(nbeams, n_samples)
  sim_nbsl_lambda2 = fltarr(nbeams, n_samples)

  if keyword_set(no_ps_plots) then plot_stdset = 0

  ;plotfile = base_path('plots') + 'power_spectrum/fhd_sim/power_ratio_vs_weight_'
  ;plotfile_freq = base_path('plots') + 'power_spectrum/fhd_sim/power_per_freq_vs_weight_'
  if keyword_set(uvf_input) then fadd = '_uvfinput' else fadd = ''
  plotfile = folder_path + 'sim_suite_plots/arrsim_flat' + '_' + run_name + '_power_vs_density' + fadd
  plotfile_uvf = folder_path + 'sim_suite_plots/arrsim_flat' + '_' + run_name + '_uvfpower_vs_density' + fadd
  plotfile_freq = folder_path + 'sim_suite_plots/arrsim_flat' + '_' + run_name + '_freq_power_vs_density' + fadd
  plotfile_wt = folder_path + 'sim_suite_plots/arrsim_flat' + '_' + run_name + '_weight_vs_density' + fadd

  suite_power_savefile = folder_path + 'sim_suite_plots/arrsim_flat' + '_' + run_name + '_cube_power_info.idlsave'

  t0 = systime(1)

  nbeams=1
  sim_beam = [1]
  for i=0, nbeams-1 do begin
    for j=0, n_samples-1 do begin

      ;; check to see if simulation exists
      if keyword_set(uvf_input) then sim_file_test = file_test(folder_path + folder_names[j]+'/*gridded_uvf.sav') $
      else sim_file_test = file_test(folder_path + folder_names[j]+'/Healpix/*cube*.sav')
      if sim_file_test eq 0 then begin
        if j eq 0 then use_saved_uvf = 0 else begin
          use_saved_uvf = 1
          input_model_files = file_search(folder_path + folder_names[0] + '/*_input_model.sav', count = nfiles)
          if nfiles eq 1 then saved_uvf_filename = input_model_files else stop
        endelse

        if keyword_set(file_uvw) then begin
          print, 'simulating file uvws in folder: ' + version[j]

          eor_simulation_enterprise, start=start_file_index, end=end_file_index, version=version[j], $
            input_uvfits_file=input_uvfits_file, input_data_directory=input_data_directory, $
            flat_sigma = flat_sigma, use_saved_uvf = use_saved_uvf, uvf_savefile = saved_uvf_filename, $
            eor_real_sky = eor_real_sky, output_directory=folder_path, restrict_hpx_inds=restrict_hpx_inds, $
            /recalculate_all; = recalc_sim
        endif else begin
          print, 'simulating density: ' + number_formatter(sample_factors[j]) + ' in folder: ' + version[j]

          eor_simulation_enterprise, start=start_file_index, end=end_file_index, version=version[j], sim_baseline_density=sample_factors[j], $
            input_uvfits_file=input_uvfits_file, input_data_directory=input_data_directory, $
            flat_sigma = flat_sigma, use_saved_uvf = use_saved_uvf, uvf_savefile = saved_uvf_filename, $
            eor_real_sky = eor_real_sky, output_directory=folder_path, restrict_hpx_inds=restrict_hpx_inds, $
            /recalculate_all; = recalc_sim
        endelse
      endif

      print, 'calculating ps for ' + folder_names[j]

      ;;if run_name eq 'noflag' then fix_use = 1 $
      ;;else if (run_name eq 'realsky' or run_name eq 'complexsky') and j gt 0 then fix_use = 1

      ps_wrapper, folder_path + folder_names[j],/sim, png = png, eps = eps, pdf = pdf, $
        refresh_ps=refresh_ps, refresh_binning = refresh_binning, $
        cube_power_info = cube_power_info, plot_stdset = plot_stdset, $
        uvf_input = uvf_input, use_weight_cutoff_sim=use_weight_cutoff_sim, $
        plot_flat_1d = flat_sigma

      sim_ave_powers[i,j] = cube_power_info.ave_power[0]
      sim_wt_ave_powers[i,j] = cube_power_info.wt_ave_power[0]
      sim_ave_powers_uvf[i,j] = cube_power_info.ave_power_uvf[0]
      sim_wt_ave_powers_uvf[i,j] = cube_power_info.wt_ave_power_uvf[0]
      sim_ave_weights[i,j] = cube_power_info.ave_weights[0]
      sim_nbsl_lambda2[i,j] = cube_power_info.nbsl_lambda2[0]
      if i eq 0 and j eq 0 then begin
        dims = size(cube_power_info.ave_power_freq, /dimension)
        sim_ave_power_freq = fltarr(nbeams, n_samples, dims[1])
        sim_wt_ave_power_freq = fltarr(nbeams, n_samples, dims[1])
        sim_ave_weights_freq = fltarr(nbeams, n_samples, dims[1])
        sim_nbsl_lambda2_freq = fltarr(nbeams, n_samples, dims[1])
      endif
      sim_ave_power_freq[i,j,*] = cube_power_info.ave_power_freq[0,*]
      sim_wt_ave_power_freq[i,j,*] = cube_power_info.wt_ave_power_freq[0,*]
      sim_ave_weights_freq[i,j,*] = cube_power_info.ave_weights_freq[0,*]
      sim_nbsl_lambda2_freq[i,j,*] = cube_power_info.nbsl_lambda2[0,*]

      ;; fix missing factor of 2 in uvf input cube
      if run_name eq 'noflag' then begin
        sim_ave_powers[i,j] *= 4.
        sim_wt_ave_powers[i,j] *= 4.
        sim_ave_powers_uvf[i,j] *= 4.
        sim_wt_ave_powers_uvf[i,j] *= 4.
        sim_ave_power_freq[i,j,*] *= 4.
        sim_wt_ave_power_freq[i,j,*] *= 4.
      endif else if run_name eq 'complexsky' then begin
        if j ne 0 then begin
          sim_ave_powers[i,j] *= 4.
          sim_wt_ave_powers[i,j] *= 4.
          sim_ave_powers_uvf[i,j] *= 4.
          sim_wt_ave_powers_uvf[i,j] *= 4.
          sim_ave_power_freq[i,j,*] *= 4.
          sim_wt_ave_power_freq[i,j,*] *= 4.
        endif
      endif

      if i+j eq 0 then flat_power = cube_power_info.flat_power else if flat_power ne cube_power_info.flat_power then print, 'flat powers do not agree'

    endfor
  endfor
  t1=systime(1)

  save, file=suite_power_savefile, sim_ave_powers, sim_wt_ave_powers, sim_ave_powers_uvf, $
    sim_wt_ave_powers_uvf, sim_ave_weights, sim_nbsl_lambda2, sim_ave_power_freq, $
    sim_wt_ave_power_freq, sim_ave_weights_freq, sim_nbsl_lambda2_freq, flat_power

  run_time = t1-t0
  if run_time lt 60 then time_str = number_formatter(run_time) + ' s' $
  else if run_time lt 3600 then time_str = number_formatter(run_time/60.) + ' m' else time_str = number_formatter(run_time/3600.) + ' h'
  print, 'run time: ' + time_str

  if keyword_set(recalculate_all) then print, sim_powers

  if not keyword_set(no_suite_plots) then begin
    colors1 = ['cyan', 'light salmon', 'light sea green', 'peru']
    colors2 = ['blue', 'red', 'sea green', 'chocolate']

    tvlct, r, g, b, /get
    position = [0.1, 0.15, 0.95, 0.9]


    if keyword_set(pub) then begin
      plotfile = plotfile + plot_exten

      charthick = 3
      thick = 3
      xthick = 3
      ythick = 3
      charsize = 2
      font = 1
      legend_charsize = 2

      if keyword_set(eps) then landscape = 0 else landscape=1
      cgps_open, plotfile, /font, encapsulated=eps, landscape=landscape, pagetype='letter'

    endif else if windowavailable(window_num) then wset, window_num else window, window_num

    ;  cgplot, sim_ave_weights[0,*], sim_ave_powers[0,*]*0+0.52, color='black', yrange = yrange, xtitle='ave weight (1/lamda^2)', ytitle = 'power ratio', xrange=xrange, $
    ;    thick = thick, charthick = charthick, xthick = xthick, ythick = ythick, charsize = charsize, font = font
    ;  for i=0, nbeams-1 do cgplot, sim_ave_weights[i,*], sim_ave_powers[i,*]/flat_power, color=colors1[i], psym=-4, /over, thick = thick
    ;  for i=0, nbeams-1 do cgplot, sim_ave_weights[i,*], sim_wt_ave_powers[i,*]/flat_power, color=colors2[i], /over, psym=-4, thick = thick
    if keyword_set(plot_ratio) then begin
      if keyword_set(use_peak) then power_norm = max(sim_wt_ave_powers) else power_norm = flat_power

      yrange=[0, 1.5]
      xrange=minmax(sim_ave_weights)

      cgplot, sim_nbsl_lambda2[0,*], sim_ave_powers[0,*]*0+0.5, color='black', yrange = yrange, $
        xtitle='baselines/lamda^2', ytitle = 'power ratio', /xlog, title = run_name, $
        thick = thick, charthick = charthick, xthick = xthick, ythick = ythick, charsize = charsize, font = font, $
        position = position
      for i=0, nbeams-1 do cgplot, sim_nbsl_lambda2[i,*], sim_wt_ave_powers[i,*]/power_norm, color=colors2[i], /over, psym=-4, thick = thick

      beam_str = strarr(nbeams)
      for i=0, nbeams-1 do begin
        case sim_beam[i] of
          1: beam_str[i] = 'zenith'
          2: beam_str[i] = 'off zenith'
        endcase
      endfor

      al_legend, ['weighted power ave ' + beam_str, '0.5'],$
        textcolor = [colors2[0:nbeams-1], 'black'], box = 0, /right, charsize = legend_charsize, charthick = charthick
    endif else begin
      yrange=minmax([sim_wt_ave_powers, + fltarr(nbeams, n_samples) + flat_power])
      yrange = 10^float([floor(alog10(yrange[0])), ceil(alog10(yrange[1]))])
      xrange=minmax(sim_nbsl_lambda2)

      cgplot, xrange, fltarr(2)+flat_power, color='black', yrange = yrange, $
        xtitle='baselines/lamda^2', ytitle = 'power', /xlog, /ylog, title = run_name, $
        thick = thick, charthick = charthick, xthick = xthick, ythick = ythick, charsize = charsize, font = font, $
        position = position
      for i=0, nbeams-1 do cgplot, sim_nbsl_lambda2[i,*], sim_wt_ave_powers[i,*], color=colors2[i], /over, psym=-4, thick = thick

      beam_str = strarr(nbeams)
      for i=0, nbeams-1 do begin
        case sim_beam[i] of
          1: beam_str[i] = 'zenith'
          2: beam_str[i] = 'off zenith'
        endcase
      endfor

      al_legend, ['weighted power ave ' + beam_str, 'input power'],$
        textcolor = [colors2[0:nbeams-1], 'black'], box = 0, /right, $
        charsize = legend_charsize, charthick = charthick

    endelse

    if keyword_set(pub) and n_elements(multi_pos) eq 0 then begin
      cgps_close, png = png, pdf = pdf, delete_ps = delete_ps, density=600

      if keyword_set(eps) then landscape = 0 else landscape=1
      cgps_open, plotfile_uvf, /font, encapsulated=eps, landscape=landscape, pagetype='letter'

    endif else if windowavailable(window_num+1) then wset, window_num+1 else window, window_num+1

    if keyword_set(plot_ratio) then begin
      if keyword_set(use_peak) then power_norm = max(sim_wt_ave_powers_uvf) else power_norm = flat_power

      ;yrange=[0, max([sim_ave_powers_uvf/power_norm, sim_wt_ave_powers_uvf/power_norm])]
      yrange=[0, max(sim_wt_ave_powers_uvf/power_norm)]

      cgplot, sim_nbsl_lambda2[0,*], sim_ave_powers_uvf[0,*]*0+0.5, color='black', yrange = yrange, $
        xtitle='baselines/lamda^2', ytitle = 'power ratio', /xlog, title = run_name, $
        thick = thick, charthick = charthick, xthick = xthick, ythick = ythick, charsize = charsize, font = font, $
        position = position
      for i=0, nbeams-1 do cgplot, sim_nbsl_lambda2[i,*], sim_wt_ave_powers_uvf[i,*]/power_norm, color=colors2[i], /over, psym=-4, thick = thick

      beam_str = strarr(nbeams)
      for i=0, nbeams-1 do begin
        case sim_beam[i] of
          1: beam_str[i] = 'zenith'
          2: beam_str[i] = 'off zenith'
        endcase
      endfor

      al_legend, ['weighted power ave (uvf) ' + beam_str, '0.5'],$
        textcolor = [colors2[0:nbeams-1], 'black'], box = 0, /right, charsize = legend_charsize, charthick = charthick
    endif else begin
      ;yrange=minmax([sim_ave_powers_uvf, sim_wt_ave_powers_uvf, + fltarr(nbeams, n_samples) + flat_power])
      yrange=minmax([sim_ave_powers_uvf, + fltarr(nbeams, n_samples) + flat_power])
      yrange = 10^float([floor(alog10(yrange[0])), ceil(alog10(yrange[1]))])
      xrange=minmax(sim_nbsl_lambda2)

      cgplot, xrange, fltarr(2)+flat_power, color='black', yrange = yrange, $
        xtitle='baselines/lamda^2', ytitle = 'power', /xlog, /ylog, title = run_name, $
        thick = thick, charthick = charthick, xthick = xthick, ythick = ythick, charsize = charsize, font = font, $
        position = position
       for i=0, nbeams-1 do cgplot, sim_nbsl_lambda2[i,*], sim_wt_ave_powers_uvf[i,*], color=colors2[i], /over, psym=-4, thick = thick

      beam_str = strarr(nbeams)
      for i=0, nbeams-1 do begin
        case sim_beam[i] of
          1: beam_str[i] = 'zenith'
          2: beam_str[i] = 'off zenith'
        endcase
      endfor

      al_legend, ['weighted power ave (uvf) ' + beam_str, 'input power'],$
        textcolor = [colors2[0:nbeams-1], 'black'], box = 0, /right, $
        charsize = legend_charsize, charthick = charthick

    endelse

    if keyword_set(pub) then begin
      cgps_close, png = png, pdf = pdf, delete_ps = delete_ps, density=600

      if keyword_set(eps) then landscape = 0 else landscape=1
      cgps_open, plotfile_wt, /font, encapsulated=eps, landscape=landscape, pagetype='letter'

    endif else if windowavailable(window_num+2) then wset, window_num+2 else window, window_num+2

    yrange=minmax(sim_ave_weights)
    yrange = 10^float([floor(alog10(yrange[0])), ceil(alog10(yrange[1]))])
    xrange=minmax(sim_nbsl_lambda2)

    cgplot, sim_nbsl_lambda2[0,*], sim_ave_weights[0,*], yrange = yrange, $
      xtitle='baselines/lamda^2', ytitle = 'ave weights', /xlog, /ylog, title = run_name, $
      thick = thick, charthick = charthick, xthick = xthick, ythick = ythick, charsize = charsize, font = font, $
        position = position
    for i=0, nbeams-1 do cgplot, sim_nbsl_lambda2[i,*], sim_ave_weights[i,*], color=colors2[i], psym=-4, /over, thick = thick

    beam_str = strarr(nbeams)
    for i=0, nbeams-1 do begin
      case sim_beam[i] of
        1: beam_str[i] = 'zenith'
        2: beam_str[i] = 'off zenith'
      endcase
    endfor

    al_legend, ['ave weights ' + beam_str],$
      textcolor = [colors2[0:nbeams-1]], box = 0, /right, $
      charsize = legend_charsize, charthick = charthick

    if keyword_set(pub) then begin
      cgps_close, png = png, pdf = pdf, delete_ps = delete_ps, density=600
    endif

    nrow = 2
    ncol = nbeams

    if keyword_set(pub) then plotfiles_use = plotfile_freq

    for i=0, nbeams-1 do begin
      if i eq 0 then begin
        start_multi_params = {ncol:ncol, nrow:nrow, ordering:'col'}
        undefine, positions, pos_use
        noerase = 0
      endif else pos_use = positions[*,2*i]

      if keyword_set(plot_ratio) then begin
        if keyword_set(use_peak) then power_norm = max(sim_wt_ave_powers_uvf) else power_norm = flat_power
      endif else power_norm = 1.

      quick_image, sim_ave_power_freq[i,*,*]/power_norm, $;sim_nbsl_lambda2[i,*], $
        start_multi_params = start_multi_params, multi_pos = pos_use, $
        xtitle = 'increasing density', ytitle = 'frequency channel', title = 'straight power ave ' + beam_str[i] + '/flat power', $
        png = png, eps = eps, pdf = pdf, alphabackgroundimage = alphabackgroundimage, savefile = plotfiles_use, noerase = noerase

      if i eq 0 then begin
        positions = pos_use
        undefine, start_multi_params
        noerase = 1
      endif

      pos_use = positions[*,2*i+1]
      quick_image, sim_wt_ave_power_freq[i,*,*]/power_norm, $;sim_nbsl_lambda2[i,*], $
        start_multi_params = start_multi_params, multi_pos = pos_use, $
        xtitle = 'increasing density', ytitle = 'frequency channel', title = 'weighted power ave ' + beam_str[i] + '/flat power', $
        png = png, eps = eps, pdf = pdf, alphabackgroundimage = alphabackgroundimage, savefile = plotfiles_use, noerase = noerase

    endfor

    if keyword_set(pub) then begin
      cgps_close, png = png, pdf = pdf, delete_ps = delete_ps, density=600
    endif



    tvlct, r, g, b
  endif

end
