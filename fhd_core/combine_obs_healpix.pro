PRO combine_obs_healpix,file_list,status_arr,hpx_inds,obs_arr,n_obs_hpx=n_obs_hpx,instr_dirty_hpx=instr_dirty_hpx,$
    instr_model_hpx=instr_model_hpx,weights_hpx=weights_hpx,instr_sources_hpx=instr_sources_hpx,$
    instr_rings_hpx=instr_rings_hpx,instr_catalog_hpx=instr_catalog_hpx,$
    nside=nside,restore_last=restore_last,output_path=output_path,$
    beam_threshold=beam_threshold,image_filter_fn=image_filter_fn,silent=silent,$
    catalog_file_path=catalog_file_path,restrict_hpx_inds=restrict_hpx_inds,_Extra=extra

except=!except
!except=0 
heap_gc
IF N_Elements(restrict_hpx_inds) EQ 0 THEN restrict_hpx_inds=0
IF N_Elements(image_filter_fn) EQ 0 THEN image_filter_fn='filter_uv_uniform'
IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=0.05

n_obs=N_Elements(file_list)
save_path=output_path+'_'+file_basename(file_list[0])+'-'+file_basename(file_list[n_obs-1])+'_maps.sav'

IF Keyword_Set(restore_last) THEN BEGIN
    RESTORE,save_path
    RETURN
ENDIF

fhd_save_io,status_init,/reset
IF N_Elements(status_arr) NE n_obs THEN BEGIN
    status_arr=Replicate(status_init,n_obs)
    FOR obs_i=0L,n_obs-1 DO BEGIN
        fhd_save_io,status_single,file_path_fhd=file_list[obs_i]
        status_arr[obs_i]=status_single
    ENDFOR
ENDIF

fi_use=where(status_arr.obs,n_obs)
IF n_obs EQ 0 THEN RETURN
file_list_use=file_list[fi_use]
status_arr_use=status_arr[fi_use]
fhd_flag=Min(status_arr.fhd)

IF N_Elements(restrict_hpx_inds) GT 1 THEN BEGIN
    hpx_inds=restrict_hpx_inds
    n_hpx=N_Elements(restrict_hpx_inds)
    fit_inds_flag=0
ENDIF ELSE BEGIN
    fit_inds_flag=1
    fhd_save_io,status_arr_use[0],obs,file_path_fhd=file_list_use[0],var='obs',/restore
    IF Keyword_Set(restrict_hpx_inds) THEN $
        IF size(restrict_hpx_inds,/type) NE 7 THEN restrict_hpx_inds_path=observation_healpix_inds_select(obs) ELSE restrict_hpx_inds_path=restrict_hpx_inds
    IF size(restrict_hpx_inds_path,/type) EQ 7 THEN BEGIN 
        file_path_use=restrict_hpx_inds_path
        IF file_test(file_path_use) EQ 0 THEN file_path_use=filepath(file_path_use,root=Rootdir('fhd'),subdir='Observations')
        
        IF  file_test(file_path_use) THEN BEGIN
            hpx_inds=getvar_savefile(file_path_use,'hpx_inds')
            nside_test=getvar_savefile(file_path_use,names=sav_contents)
            IF Max(strmatch(StrLowCase(sav_contents),'nside')) EQ 1 THEN nside=getvar_savefile(file_path_use,'nside') ELSE BEGIN
                max_ind=Max(hpx_inds)
                IF Keyword_Set(nside) THEN nside=(2.^(Ceil(ALOG(Sqrt(max_ind/12.))/ALOG(2))))>nside ELSE nside=2.^(Ceil(ALOG(Sqrt(max_ind/12.))/ALOG(2))) 
            ENDELSE
        ENDIF
    ENDIF
ENDELSE

FOR obs_i=0,n_obs-1 DO BEGIN
    file_path_fhd=file_list_use[obs_i]
    fhd_save_io,status_arr_use[obs_i],obs,file_path_fhd=file_path_fhd,var='obs',/restore
    IF obs_i EQ 0 THEN obs_arr=[obs] ELSE obs_arr=[obs_arr,obs]
    
    beam_width=beam_width_calculate(obs)
;    beam_area=2.*!Pi*(beam_width*obs.degpix)^2. 
    beam_area=(beam_width*obs.degpix)^2.
    pix_sky=4.*!Pi*!RaDeg^2./beam_area
    Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
    Nside_chk*=2.
    
    IF ~Keyword_Set(nside) THEN nside_use=Nside_chk ELSE nside_use=nside
    nside_use=nside_use>Nside_chk
ENDFOR

n_pol=Min(obs_arr.n_pol)
IF Keyword_Set(nside) THEN nside_use=nside ELSE nside=nside_use
IF ~Keyword_Set(silent) THEN print,'Creating HEALPix maps using nside='+Strn(nside)

instr_model_hpx=Ptrarr(n_pol)
instr_dirty_hpx=Ptrarr(n_pol)
instr_sources_hpx=Ptrarr(n_pol)
instr_rings_hpx=Ptrarr(n_pol)
instr_catalog_hpx=Ptrarr(n_pol)
weights_hpx=Ptrarr(n_pol)

FOR obs_i=0L,n_obs-1 DO BEGIN
    file_path_fhd=file_list_use[obs_i]
    IF ~Keyword_Set(silent) THEN print,StrCompress('Converting '+file_basename(file_path_fhd)+'('+Strn(obs_i+1)+' of '+Strn(n_obs)+')')
    obs=obs_arr[obs_i]
    status_str=status_arr_use[obs_i]
    dimension=obs.dimension
    elements=obs.elements
    n_vis_rel=obs.n_vis/Mean(obs_arr.n_vis)
    astr=obs.astr            
    restored_beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)/(2.*Sqrt(2.*Alog(2.)))
    fhd_save_io,status_str,cal,file_path_fhd=file_path_fhd,var='cal',/restore
    IF N_Elements(cal) EQ 0 THEN cal=fhd_struct_init_cal(obs,file_path_fhd=file_path_fhd)
    
    image_uv_arr=Ptrarr(n_pol)
    weights_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,grid_uv,var='grid_uv',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i
        IF N_Elements(grid_uv) GT 0 THEN image_uv_arr[pol_i]=Ptr_new(grid_uv)
        fhd_save_io,status_str,weights_uv,var='weights_uv',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i
        IF N_Elements(weights_uv) GT 0 THEN weights_arr[pol_i]=Ptr_new(weights_uv)
        undefine_fhd,grid_uv,weights_uv
    ENDFOR
    
    IF fhd_flag THEN BEGIN
        fhd_save_io,status_str,source_array,var='fhd',/restore,file_path_fhd=file_path_fhd,sub_var='source_array'
        fhd_save_io,status_str,model_uv_holo,var='fhd',/restore,file_path_fhd=file_path_fhd,sub_var='model_uv_holo'
        fhd_save_io,status_str,beam_base,var='fhd',/restore,file_path_fhd=file_path_fhd,sub_var='beam_base'
        beam_base2=Ptrarr(n_pol)
        FOR pol_i=0,n_pol-1 DO beam_base2[pol_i]=Ptr_new((*beam_base[pol_i])^2.)
        model_flag=1
        source_flag=1
    ENDIF ELSE BEGIN
        model_uv_holo=Ptrarr(n_pol)
        model_flag=Min(status_str.grid_uv_model[0:n_pol-1])
        IF model_flag THEN FOR pol_i=0,n_pol-1 DO BEGIN
            fhd_save_io,status_str,grid_uv_model,var='grid_uv_model',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i
            IF N_Elements(grid_uv_model) GT 0 THEN model_uv_holo[pol_i]=Ptr_new(grid_uv_model)
            undefine_fhd,grid_uv_model
        ENDFOR
        IF cal.n_cal_src GT 0 THEN BEGIN
            source_flag=1
            source_array=cal.source_list
        ENDIF ELSE source_flag=0
        
        IF status_str.psf THEN fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd $
            ELSE psf=beam_setup(obs,status_str,file_path_fhd=file_path_fhd,restore_last=0,silent=1,no_save=1,_Extra=extra)
        beam_base=Ptrarr(n_pol)
        FOR pol_i=0,n_pol-1 DO beam_base[pol_i]=Ptr_new(beam_image(psf,obs,pol_i=pol_i,square=0))
        beam_base2=Ptrarr(n_pol)
        FOR pol_i=0,n_pol-1 DO beam_base2[pol_i]=Ptr_new((*beam_base[pol_i])^2.)
    ENDELSE
    beam_mask=fltarr(dimension,elements)+1.
    FOR pol_i=0,(n_pol<2)-1 DO BEGIN
        mask0=fltarr(dimension,elements)
        mask_i=region_grow(*beam_base[pol_i],Floor(obs.obsx)+dimension*Floor(obs.obsy),thresh=[beam_threshold,max(*beam_base[pol_i])])
        mask0[mask_i]=1
        beam_mask*=mask0
    ENDFOR
    
    IF status_str.jones THEN fhd_save_io,status_str,jones,var='jones',/restore,file_path_fhd=file_path_fhd $
        ELSE jones=fhd_struct_init_jones(obs,status_str,file_path_fhd=file_path_fhd,restore=0,mask=beam_mask)
    IF source_flag THEN source_array=Stokes_Cnv(source_array,jones,obs,/inverse)
    instr_model_arr=Ptrarr(n_pol)
    instr_dirty_arr=Ptrarr(n_pol)
    instr_sources=Ptrarr(n_pol)
    instr_rings=Ptrarr(n_pol)
    filter_arr=Ptrarr(n_pol,/allocate) 
    FOR pol_i=0,n_pol-1 DO BEGIN
        ;we want ALL images in the beam^2 holographic frame
        instr_dirty_arr[pol_i]=Ptr_new(dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix,weights=*weights_arr[pol_i],/antialias,$
            image_filter_fn=image_filter_fn,file_path_fhd=file_path_fhd,filter=filter_arr[pol_i],_Extra=extra))
        IF model_flag THEN instr_model_arr[pol_i]=Ptr_new(dirty_image_generate(*model_uv_holo[pol_i],degpix=degpix,weights=*weights_arr[pol_i],/antialias,$
            image_filter_fn=image_filter_fn,file_path_fhd=file_path_fhd,filter=filter_arr[pol_i],_Extra=extra))
        IF source_flag THEN BEGIN
            IF Keyword_Set(ring_radius) THEN instr_rings[pol_i]=Ptr_new(source_image_generate(source_array,obs,pol_i=pol_i,resolution=16.,$
                dimension=dimension,restored_beam_width=restored_beam_width,ring_radius=ring_radius,_Extra=extra)*(*beam_base[pol_i]))
            instr_sources[pol_i]=Ptr_new(source_image_generate(source_array,obs,pol_i=pol_i,resolution=16.,$
                dimension=dimension,restored_beam_width=restored_beam_width,_Extra=extra)*(*beam_base[pol_i]))
        ENDIF
    ENDFOR
    
    ; renormalize based on weights
    renorm_factor = get_image_renormalization(obs,weights_arr=weights_arr,beam_base=beam_base,filter_arr=filter_arr,$
        image_filter_fn=image_filter_fn,degpix=degpix,/antialias)
    FOR pol_i=0,n_pol-1 DO BEGIN
        *instr_dirty_arr[pol_i]*=renorm_factor*n_vis_rel
        IF model_flag THEN *instr_model_arr[pol_i]*=renorm_factor*n_vis_rel 
        IF source_flag THEN BEGIN
             IF Keyword_Set(ring_radius) THEN *instr_rings[pol_i]*=n_vis_rel
             *instr_sources[pol_i]*=n_vis_rel
        ENDIF
        *beam_base[pol_i]*=n_vis_rel
        *beam_base2[pol_i]*=n_vis_rel
    ENDFOR
    
    hpx_cnv=healpix_cnv_generate(obs,status_str,file_path_fhd=file_path_fhd,nside=nside,$
        mask=beam_mask,restore_last=0,restrict_hpx_inds=restrict_hpx_inds,/no_save,_Extra=extra)
    hpx_inds1=hpx_cnv.inds 
    
    IF fit_inds_flag THEN BEGIN
        IF N_Elements(hpx_inds) EQ 0 THEN BEGIN
            hpx_inds=(hpx_inds1)
            IF Keyword_Set(restrict_hpx_inds) THEN BEGIN
                restrict_hpx_inds=hpx_inds
                fit_inds_flag=0
            ENDIF
            N_hpx=N_Elements(hpx_inds)
            ind_map1=Lindgen(N_hpx) ;index map of hpx_inds that correspond to hpx_inds1 
            reform_flag=0
        ENDIF ELSE BEGIN
            hist_min=Min(hpx_inds)<Min(hpx_inds1)
            hist_max=Max(hpx_inds)>Max(hpx_inds1)
            hist0=histogram(hpx_inds,min=hist_min,max=hist_max,/binsize,reverse_ind=ri0)
            hist1=histogram(hpx_inds1,min=hist_min,max=hist_max,/binsize,reverse_ind=ri1)
            hist=hist0+hist1
            hpx_inds_i=where(hist,n_hpx) ;reduce all sky healpix indices to sparse format
;            hist0=hist0[hpx_inds_i]
;            hist1=hist1[hpx_inds_i]
            ind_use0=where(hist0[hpx_inds_i],n_hpx0) ;determine existing healpix sparse format pixels in combined sparse format
            ind_use1=where(hist1[hpx_inds_i],n_hpx1) ;determine new healpix sparse format pixels in combined sparse format
            
            IF n_hpx0 EQ n_hpx THEN reform_flag=0 ELSE BEGIN
                reform_flag=1
                ind_use0b=ri0[ri0[hpx_inds_i[ind_use0]]]
                ind_order0=Sort(ind_use0b) ;determine ordering needed to convert monotonic increasing sparse indices to actual ordering of healpix pixels (in case of RA=0 branch cut)
                ind_map0=ind_use0[ind_order0] 
;                ri0=0               
            ENDELSE
            ind_use1b=ri1[ri1[hpx_inds_i[ind_use1]]]
            ind_order1=Sort(ind_use1b)
            ind_map1=ind_use1[ind_order1]
;            ind_order1=Sort(Sort(ri1[ri1[hpx_inds_i[ind_use1]]])) ;determine ordering needed to convert monotonic increasing sparse indices to actual ordering of healpix pixels (in case of RA=0 branch cut)
;;            ri1=0
;            ind_map1=ind_use1[ind_order1]
            hpx_inds=hpx_inds_i+hist_min
        ENDELSE
    ENDIF ELSE BEGIN
    ;This option is not debugged!
        hist_min=Min(hpx_inds)<Min(hpx_inds1)
        hist_max=Max(hpx_inds)>Max(hpx_inds1)
        hist0=histogram((hpx_inds),min=hist_min,max=hist_max,/binsize)
        hist1=histogram((hpx_inds1),min=hist_min,max=hist_max,/binsize,reverse_ind=ri1)
;        hist=hist0+hist1 ; in this case, ONLY want indices found in the input hpx_inds
        hpx_inds_i=where(hist0 AND hist1,n_hpx)
        hist1=hist1[hpx_inds_i]
        ind_use1=where((hist1),n_hpx1)
        ind_order1=Sort(ri1[ri1[hpx_inds_i[ind_use1]]])
        ind_map1=ind_use1[ind_order1]
        reform_flag=0
    ENDELSE
    
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF ~Ptr_valid(instr_dirty_hpx[pol_i]) THEN instr_dirty_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
        IF model_flag THEN IF ~Ptr_valid(instr_model_hpx[pol_i]) THEN instr_model_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
        IF source_flag THEN BEGIN
            IF Keyword_Set(ring_radius) THEN IF ~Ptr_valid(instr_rings_hpx[pol_i]) THEN instr_rings_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
            IF ~Ptr_valid(instr_sources_hpx[pol_i]) THEN instr_sources_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
        ENDIF
        IF ~Ptr_valid(weights_hpx[pol_i]) THEN weights_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
        IF reform_flag THEN BEGIN
            ;if reform_flag is set, that means that the latest observation has added new healpix pixels, so the old collection of pixels needs to be expanded 
            instr_dirty_hpx0=Fltarr(n_hpx)
            instr_dirty_hpx0[ind_map0]=(*instr_dirty_hpx[pol_i])
            *instr_dirty_hpx[pol_i]=(instr_dirty_hpx0)
            
            IF model_flag THEN BEGIN
                instr_model_hpx0=Fltarr(n_hpx)
                instr_model_hpx0[ind_map0]=(*instr_model_hpx[pol_i])
                *instr_model_hpx[pol_i]=(instr_model_hpx0)
            ENDIF
            
            IF source_flag THEN BEGIN
                IF Keyword_Set(ring_radius) THEN BEGIN 
                    instr_rings_hpx0=Fltarr(n_hpx)
                    instr_rings_hpx0[ind_map0]=(*instr_rings_hpx[pol_i])
                    *instr_rings_hpx[pol_i]=(instr_rings_hpx0)
                ENDIF
                instr_sources_hpx0=Fltarr(n_hpx)
                instr_sources_hpx0[ind_map0]=(*instr_sources_hpx[pol_i])
                *instr_sources_hpx[pol_i]=(instr_sources_hpx0)
            ENDIF
            weights_hpx0=Fltarr(n_hpx)
            weights_hpx0[ind_map0]=(*weights_hpx[pol_i])
            *weights_hpx[pol_i]=(weights_hpx0)
        ENDIF 
        (*weights_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*beam_base2[pol_i],hpx_cnv)
        (*instr_dirty_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*instr_dirty_arr[pol_i],hpx_cnv)
        IF model_flag THEN (*instr_model_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*instr_model_arr[pol_i],hpx_cnv)
        IF source_flag THEN BEGIN
            IF Keyword_Set(ring_radius) THEN (*instr_rings_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*instr_rings[pol_i],hpx_cnv)
            (*instr_sources_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*instr_sources[pol_i],hpx_cnv)
        ENDIF
    ENDFOR
    IF N_Elements(n_obs_hpx) EQ 0 THEN n_obs_hpx=intarr(n_hpx)
    IF reform_flag THEN BEGIN
        n_obs_hpx0=intarr(n_hpx)
        n_obs_hpx0[ind_map0]=n_obs_hpx
        n_obs_hpx=(n_obs_hpx0)
    ENDIF
    n_obs_hpx[ind_map1]+=1
    
    ri0=0
    ri1=0
    undefine_fhd,instr_model_arr,instr_dirty_arr,instr_sources,instr_rings,filter_arr,hpx_cnv,beam_base2,beam_base,jones
ENDFOR

SAVE,hpx_inds,nside,obs_arr,n_obs_hpx,instr_dirty_hpx,instr_model_hpx,weights_hpx,$
    instr_sources_hpx,instr_rings_hpx,instr_catalog_hpx,filename=save_path,/compress
END
