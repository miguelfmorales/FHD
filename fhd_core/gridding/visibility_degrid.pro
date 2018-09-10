FUNCTION visibility_degrid,image_uv,vis_weight_ptr,obs,psf,params,$
    timing=timing,polarization=polarization,silent=silent,$
    complex=complex,fill_model_visibilities=fill_model_visibilities,$
    vis_input_ptr=vis_input_ptr,spectral_model_uv_arr=spectral_model_uv_arr,$
    beam_mask_threshold=beam_mask_threshold,majick_beam=majick_beam,$
    interpolate_beam_threshold=interpolate_beam_threshold,$
    uv_grid_phase_only=uv_grid_phase_only,_Extra=extra
t0=Systime(1)
heap_gc

pol_names=obs.pol_names
complex=psf.complex_flag
n_spectral=obs.degrid_spectral_terms
double_precision=0
IF Tag_Exist(obs, 'double_precision') THEN double_precision=obs.double_precision
IF Tag_exist(psf,'interpolate_kernel') THEN interp_flag=psf.interpolate_kernel ELSE interp_flag=0

;extract information from the structures
dimension=Float(obs.dimension)
elements=Float(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline

freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=psf.n_freq
bin_offset=(*obs.baseline_info).bin_offset
frequency_array=(*obs.baseline_info).freq
freq_delta=(frequency_array-obs.freq_center)/obs.freq_center

psf_dim=psf.dim
psf_resolution=Long(psf.resolution)

vis_weight_switch=Ptr_valid(vis_weight_ptr)
uu=params.uu
vv=params.vv
ww=params.ww
kx_arr=uu/kbinsize
ky_arr=vv/kbinsize
nbaselines=obs.nbaselines
n_samples=obs.n_time
n_freq_use=N_Elements(frequency_array)
n_freq=Long(obs.n_freq)
n_freq_bin=N_Elements(freq_bin_i)
psf_dim2=2*psf_dim
group_arr=reform(psf.id[polarization,freq_bin_i,*])
beam_arr=*psf.beam_ptr


if keyword_set(majick_beam) then begin
    uv_grid_phase_only=1
    psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
    IF N_Elements(psf_image_resolution) EQ 0 THEN psf_image_resolution=1. ;no need to pad image with Majick
    IF N_Elements(beam_mask_threshold) EQ 0 THEN beam_mask_threshold=1E2
    psf_image_dim=psf_dim*psf_image_resolution*psf_intermediate_res
    psf_scale=obs.dimension*psf_intermediate_res/psf_image_dim

    image_bot=-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)
    image_top=(psf_dim*psf_resolution-1)-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)

    ;Calculate RA,DEC of pixel centers for image-based phasing
    ;Based off of Jack Line's thesis work
    xvals_celestial=meshgrid(psf_image_dim,psf_image_dim,1)*psf_scale-psf_image_dim*psf_scale/2.+obs.obsx
    yvals_celestial=meshgrid(psf_image_dim,psf_image_dim,2)*psf_scale-psf_image_dim*psf_scale/2.+obs.obsy
    apply_astrometry, obs, x_arr=xvals_celestial, y_arr=yvals_celestial, ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad

    ;Calculate l mode, m mode, and phase-tracked n mode of pixel centers
    cdec0 = cos(obs.obsdec*!dtor)
    sdec0 = sin(obs.obsdec*!dtor)
    cdec = cos(dec_arr*!dtor)
    sdec = sin(dec_arr*!dtor)
    cdra = cos((ra_arr-obs.obsra)*!dtor)
    sdra = sin((ra_arr-obs.obsra)*!dtor)
    l_mode = cdec*sdra
    m_mode = sdec*cdec0 - cdec*sdec0*cdra
    ;n=1 at phase center, so reference from there for phase tracking
    n_tracked = (sdec*sdec0 + cdec*cdec0*cdra) - 1.
    infinite_vals=where(NOT float(finite(n_tracked)),n_count)
    n_tracked[infinite_vals]=0
    l_mode[infinite_vals]=0
    m_mode[infinite_vals]=0

    if keyword_set(uv_grid_phase_only) then n_tracked[*]=0
endif

vis_dimension=nbaselines*n_samples
IF Keyword_Set(double_precision) THEN visibility_array=DComplexarr(n_freq,vis_dimension) $
    ELSE visibility_array=Complexarr(n_freq,vis_dimension) 

dist_test=Sqrt((kx_arr)^2.+(ky_arr)^2.)*kbinsize
dist_test=Float(frequency_array#dist_test)
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
dist_test=0

conj_i=where(ky_arr GT 0,n_conj)
conj_flag=intarr(N_Elements(ky_arr)) 
IF n_conj GT 0 THEN BEGIN
    conj_flag[conj_i]=1
    kx_arr[conj_i]=-kx_arr[conj_i]
    ky_arr[conj_i]=-ky_arr[conj_i]
    uu[conj_i]=-uu[conj_i]
    vv[conj_i]=-vv[conj_i]
    ww[conj_i]=-ww[conj_i]
ENDIF

xcen=Float(frequency_array#kx_arr)
ycen=Float(frequency_array#ky_arr)
 
x = (FINDGEN(dimension) - dimension/2.)*obs.kpix
y = (FINDGEN(dimension) - dimension/2.)*obs.kpix

x_offset=Fix(Floor((xcen-Floor(xcen))*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
y_offset=Fix(Floor((ycen-Floor(ycen))*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
dx_arr = (xcen-Floor(xcen))*psf_resolution - Floor((xcen-Floor(xcen))*psf_resolution)
dy_arr = (ycen-Floor(ycen))*psf_resolution - Floor((ycen-Floor(ycen))*psf_resolution)
dx0dy0_arr = (1-dx_arr)*(1-dy_arr)
dx0dy1_arr = (1-dx_arr)*dy_arr
dx1dy0_arr = dx_arr*(1-dy_arr)
dx1dy1_arr = Temporary(dx_arr) * Temporary(dy_arr)
xmin=Long(Floor(Temporary(xcen))+dimension/2.-(psf_dim/2.-1))
ymin=Long(Floor(Temporary(ycen))+elements/2.-(psf_dim/2.-1))

range_test_x_i=where((xmin LE 0) OR ((xmin+psf_dim-1) GE dimension-1),n_test_x)
range_test_y_i=where((ymin LE 0) OR ((ymin+psf_dim-1) GE elements-1),n_test_y)

IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)

;IF n_dist_flag GT 0 THEN BEGIN
;    xmin[flag_dist_i]=-1
;    ymin[flag_dist_i]=-1
;ENDIF

IF vis_weight_switch THEN BEGIN
    flag_i=where(*vis_weight_ptr LE 0,n_flag)
    IF Keyword_Set(fill_model_visibilities) THEN n_flag=0L
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
    flag_i=0
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=Long(histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0)) ;should miss any (xmin,ymin)=(-1,-1) from weights
bin_i=Long(where(bin_n,n_bin_use));+bin_min

ind_ref=Lindgen(max(bin_n))

CASE 1 OF
    Keyword_Set(complex) AND Keyword_Set(double_precision): init_arr=Dcomplexarr(psf_dim2,psf_dim2)
    Keyword_Set(double_precision): init_arr=Dblarr(psf_dim2,psf_dim2)
    Keyword_Set(complex): init_arr=Complexarr(psf_dim2,psf_dim2)
    ELSE: init_arr=Fltarr(psf_dim2,psf_dim2)
ENDCASE
arr_type=Size(init_arr,/type)

time_check_interval=Ceil(n_bin_use/10.)
t1=0
t2=0
t3=0
t4=0
t5=0
image_uv_use=image_uv
psf_dim3=Long(psf_dim*psf_dim)

;pdim=size(psf_base,/dimension)
;psf_base_dag=Ptrarr(pdim,/allocate)
;FOR pdim_i=0L,Product(pdim)-1 DO *psf_base_dag[pdim_i]=Conj(*psf_base[pdim_i])
IF Keyword_Set(n_spectral) THEN BEGIN
    prefactor=Ptrarr(n_spectral)
    FOR s_i=0,n_spectral-1 DO prefactor[s_i]=Ptr_new(deriv_coefficients(s_i+1,/divide_factorial))
    box_arr_ptr=Ptrarr(n_spectral)
ENDIF

if keyword_set(majick_beam) then begin
  IF N_Elements(beam_mask_threshold) EQ 0 THEN beam_mask_threshold=1E2
  beam_norm=1.
  beam_int=FLTARR(n_freq)
  beam2_int=FLTARR(n_freq)
  n_grp_use=FLTARR(n_freq) 
  primary_beam_area=ptr_new(Fltarr(n_freq))
  primary_beam_sq_area=ptr_new(Fltarr(n_freq))
  pol_arr=[[0,0],[1,1],[0,1],[1,0]]
  ant_pol1=pol_arr[0,polarization]
  ant_pol2=pol_arr[1,polarization]
endif

FOR bi=0L,n_bin_use-1 DO BEGIN
    t1_0=Systime(1)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    x_off=x_offset[inds]
    y_off=y_offset[inds]
    dx1dy1 = dx1dy1_arr[inds]
    dx1dy0 = dx1dy0_arr[inds]
    dx0dy1 = dx0dy1_arr[inds]
    dx0dy0 = dx0dy0_arr[inds]
        
    xmin_use=xmin[ind0] ;should all be the same, but don't want an array
    ymin_use=ymin[ind0] ;should all be the same, but don't want an array

    freq_i=(inds mod n_freq_use)
    fbin=freq_bin_i[freq_i]
     
    vis_n=bin_n[bin_i[bi]]
    baseline_inds=Floor(inds/n_freq_use) mod nbaselines
    group_id=group_arr[inds]
    group_max=Max(group_id)+1
    
;    psf_conj_flag=intarr(vis_n)
;    IF n_conj GT 0 THEN BEGIN
;        bi_vals=Floor(inds/n_freq_use)
;        psf_conj_flag=conj_flag[bi_vals]
;    ENDIF 
    
    IF interp_flag THEN n_xyf_bin=vis_n ELSE BEGIN
        xyf_i=(x_off+y_off*psf_resolution+fbin*psf_resolution^2.)*group_max+group_id
        xyf_si=Sort(xyf_i)
        xyf_i=xyf_i[xyf_si]
        xyf_ui=Uniq(xyf_i)
        n_xyf_bin=N_Elements(xyf_ui)
    ENDELSE
    
    IF (vis_n GT Ceil(1.1*n_xyf_bin)) AND ~keyword_set(majick_beam) THEN BEGIN ;there might be a better selection criteria to determine which is most efficient
        ind_remap_flag=1
        
        inds=inds[xyf_si]
        inds_use=[xyf_si[xyf_ui]]
        
        freq_i=freq_i[inds_use]
        x_off=x_off[inds_use] 
        y_off=y_off[inds_use]
        fbin=fbin[inds_use]
        baseline_inds=baseline_inds[inds_use]
;        psf_conj_flag=psf_conj_flag[inds_use]
        
        IF n_xyf_bin EQ 1 THEN ind_remap=intarr(vis_n) ELSE BEGIN
            hist_inds_u=histogram(xyf_ui,/binsize,min=0,reverse_ind=ri_xyf)
            ind_remap=ind_ref[ri_xyf[0:n_elements(hist_inds_u)-1]-ri_xyf[0]]
        ENDELSE
        
        vis_n=Long64(n_xyf_bin)
    ENDIF ELSE BEGIN
        ind_remap_flag=0
        bt_index = inds / n_freq_use
    ENDELSE
    
    box_matrix=Make_array(psf_dim3,vis_n,type=arr_type) 
    
    box_arr=Reform(image_uv_use[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1],psf_dim3)
    t3_0=Systime(1)
    t2+=t3_0-t1_0

    ;Make the beams on the fly with corrective phases given the baseline location
    if keyword_set(majick_beam) then begin
        FOR ii=0L,vis_n-1 DO begin
            ;Pixel center offset phases
            deltau_l = l_mode*(uu[bt_index[ii]]*frequency_array[freq_i[ii]]-x[xmin_use+psf_dim/2])
            deltav_m = m_mode*(vv[bt_index[ii]]*frequency_array[freq_i[ii]]-y[ymin_use+psf_dim/2])
            ;w term offset phase
            w_n_tracked = n_tracked*ww[bt_index[ii]]*frequency_array[freq_i[ii]]
            
            ;Generate a UV beam from the image space beam, offset by calculated phases
            psf_base_superres=dirty_image_generate((*psf.image_power_beam_arr[polarization,fbin[ii]])*$
              exp(2.*!pi*Complex(0,1)*(-w_n_tracked+deltau_l+deltav_m)),/no_real)
            
            psf_base_superres=psf_base_superres[image_bot:image_top,image_bot:image_top]
            d = size(psf_base_superres,/DIMENSIONS) & nx = d[0]/2 & ny = d[1]/2
            ;A quick way to sum down the image by a factor of 2 in both dimensions.
            ;  indices of all the 2x2 sub-arrays are next to each other in memory
            ;  then, total collapses the 2x2 sub-arrays
            psf_base_superres = transpose(total(reform(transpose(reform(psf_base_superres,2,nx,2*ny),$
              [0,2,1]), 4,ny,nx),1))

            psf_base_superres = reform(psf_base_superres, psf.dim^2.)
            box_matrix[psf_dim3*ii]=psf_base_superres
        endfor
        psf_val_ref=Total(box_matrix)
        psf_amp = abs(box_matrix)
        psf_mask_threshold_use = Max(psf_amp)/beam_mask_threshold
        psf_phase = Atan(box_matrix, /phase)
        psf_amp -= psf_mask_threshold_use
        box_matrix = psf_amp*Cos(psf_phase) + Complex(0,1)*psf_amp*Sin(psf_phase)
        small_inds=where(psf_amp LT 0, n_count) ; should be max by kernel, but this is fast
        if n_count GT 0 then box_matrix[small_inds]=0
        box_matrix*=psf_val_ref/Total(box_matrix)
        beam_int_temp = Total(box_matrix,1,/double)/psf_resolution^2.
        beam2_int_temp = Total(Abs(box_matrix)^2,1,/double)/psf_resolution^2.
        for ii=0, N_elements(freq_i)-1 do begin
          beam_int[freq_i[ii]]+=beam_int_temp[ii]
          beam2_int[freq_i[ii]]+=beam2_int_temp[ii]  
          n_grp_use[freq_i[ii]]+=1
        endfor
    endif else begin
        IF interp_flag THEN $
          FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=$
            interpolate_kernel(*beam_arr[polarization,fbin[ii],baseline_inds[ii]],x_offset=x_off[ii], y_offset=y_off[ii],dx0dy0=dx0dy0[ii], dx1dy0=dx1dy0[ii], dx0dy1=dx0dy1[ii], dx1dy1=dx1dy1[ii]) $
        ELSE FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=*(*beam_arr[polarization,fbin[ii],baseline_inds[ii]])[x_off[ii],y_off[ii]] ;more efficient array subscript notation
    endelse

    t4_0=Systime(1)
    t3+=t4_0-t3_0
    IF Keyword_Set(n_spectral) THEN BEGIN
        vis_box=matrix_multiply(box_matrix,Temporary(box_arr),/atranspose)
        freq_term_arr=Rebin(transpose(freq_delta[freq_i]),psf_dim3,vis_n,/sample)
        FOR s_i=0,n_spectral-1 DO BEGIN
            ;s_i loop is over terms of the Taylor expansion, starting from the lowest-order term
            prefactor_use=*prefactor[s_i]
            box_matrix*=freq_term_arr
            box_arr_ptr[s_i]=Ptr_new(Reform((*spectral_model_uv_arr[s_i])[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1],psf_dim3))
            
            FOR s_i_i=0,s_i DO BEGIN
                ;s_i_i loop is over powers of the model x alpha^n, n=s_i_i+1
                degree=n_spectral
                box_arr=prefactor_use[s_i_i]*(*box_arr_ptr[s_i_i])
                vis_box+=matrix_multiply(box_matrix,Temporary(box_arr),/atranspose)
            ENDFOR
        ENDFOR
        ptr_free,box_arr_ptr
    ENDIF ELSE vis_box=matrix_multiply(Temporary(box_matrix),Temporary(box_arr),/atranspose) ;box_matrix#box_arr
    t5_0=Systime(1)
    t4+=t5_0-t4_0
    IF ind_remap_flag THEN vis_box=vis_box[ind_remap]
    visibility_array[inds]=vis_box
    
    t5_1=Systime(1)
    t5+=t5_1-t5_0
    t1+=t5_1-t1_0 
ENDFOR

if keyword_set(majick_beam) then begin
    beam2_int*=weight_invert(n_grp_use)/kbinsize^2. ;factor of kbinsize^2 is FFT units normalization
    beam_int*=weight_invert(n_grp_use)/kbinsize^2.
    (*primary_beam_sq_area)=Float(beam2_int)
    (*primary_beam_area)=Float(beam_int)
    obs.primary_beam_area[polarization]=primary_beam_area
    obs.primary_beam_sq_area[polarization]=primary_beam_sq_area
endif

x_offset=(y_offset=0)
xmin=(ymin=0)
bin_n=0
IF n_conj GT 0 THEN BEGIN
    visibility_array[*,conj_i]=Conj(visibility_array[*,conj_i])
ENDIF

IF Ptr_valid(vis_input_ptr) THEN IF N_Elements(*vis_input_ptr) EQ N_Elements(visibility_array) THEN BEGIN
    vis_return=vis_input_ptr
    *vis_return+=Temporary(visibility_array)
ENDIF
IF ~Ptr_valid(vis_return) THEN vis_return=Ptr_new(visibility_array,/no_copy)

timing=Systime(1)-t0
IF not Keyword_Set(silent) THEN print,timing,t1,t2,t3,t4,t5
RETURN,vis_return
END
