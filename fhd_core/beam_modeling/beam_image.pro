;+
; :Description:
;    Generates the average beam image for one polarization.
;    Note that the UV->sky transformation uses the inverse FFT for the beam but the forward FFT for the image.
;    This convention ensures the correct orientation of the UV-space beam for gridding visibilities.
;
; :Params:
;    psf_base_ptr - equal to psf.base standard structure.
;
; :Keywords:
;    pol_i - polarization index. 0:XX, 1:YY, 2:XY, 3:YX
;    
;    freq_i - If set, returns the beam of a specific frequency bin instead of the average beam.
;    
;    dimension - size of image in pixels. If elements is also set, this refers to the size of the first dimension
;    
;    elements - defaults to elements=dimension
;
; :Author: isullivan May 4, 2012
;-
FUNCTION beam_image,psf,obs,pol_i=pol_i,freq_i=freq_i,dimension=dimension,elements=elements,abs=abs,square=square
compile_opt idl2,strictarrsubs  

IF N_Elements(pol_i) EQ 0 THEN pol_i=0
IF N_Elements(dimension) EQ 0 THEN dimension=obs.dimension
IF N_Elements(elements) EQ 0 THEN elements=dimension
psf_dim=psf.dim
psf_res=psf.resolution
n_pol=psf.n_pol
n_freq=psf.n_freq
pol_norm=psf.pnorm
freq_norm=psf.fnorm
rbin=0;psf_res/2
xl=dimension/2-psf_dim/2+1
xh=dimension/2-psf_dim/2+psf_dim
yl=elements/2-psf_dim/2+1
yh=elements/2-psf_dim/2+psf_dim

group_id=psf.id[pol_i,0,*]
group_n=histogram(group_id,min=0,/binsize,reverse_ind=ri_id)
gi_use=where(group_n,n_groups)
gi_ref=ri_id[ri_id[gi_use]]

IF tag_exist(psf,'beam_gaussian_params') THEN $
  beam_gaussian_params=psf.beam_gaussian_params else beam_gaussian_params=0
IF tag_exist(psf,'fbin_i') THEN freq_bin_i=psf.fbin_i

if ~keyword_set(beam_gaussian_params) then beam_arr=*psf.beam_ptr

IF Keyword_Set(obs) THEN BEGIN
    IF Tag_exist(obs,'fbin_i') THEN freq_bin_i=obs.fbin_i ELSE freq_bin_i=(*obs.baseline_info).fbin_i
    n_freq=obs.n_freq
    IF tag_exist((*obs.baseline_info),'freq_use') THEN freq_i_use=where((*obs.baseline_info).freq_use GT 0) $
        ELSE freq_i_use=findgen(n_freq)
ENDIF

IF N_Elements(freq_i) GT 0 THEN freq_i_use=freq_i

n_bin_use=0.
IF Keyword_Set(square) THEN BEGIN
    beam_base=Fltarr(dimension,elements)

    ;backwards compatibility
    IF N_Elements(freq_bin_i) EQ 0 THEN BEGIN
        n_freq_bin=psf.n_freq
        FOR fi=0,n_freq_bin-1 DO BEGIN
            beam_single=Complexarr(psf_dim,psf_dim)
            FOR gi=0,n_groups-1 DO BEGIN
                beam_single+=Reform(*(*beam_arr[pol_i,fi,gi_ref[gi]])[rbin,rbin]*group_n[gi_use[gi]],psf_dim,psf_dim)
            ENDFOR
            beam_single/=Total(group_n[gi_use])
            IF Keyword_Set(abs) THEN beam_single=Abs(beam_single)
            beam_base_uv1=Complexarr(dimension,elements)
            beam_base_uv1[xl:xh,yl:yh]=beam_single
;            beam_base_uv1+=Shift(Reverse(reverse(Conj(beam_base_uv1),1),2),1,1)
            beam_base_single=fft_shift(FFT(fft_shift(beam_base_uv1),/inverse));/2.
            beam_base+=Real_part(beam_base_single*Conj(beam_base_single));>0
            n_bin_use+=1.*freq_norm[fi]
        ENDFOR
    ENDIF ELSE BEGIN
        IF N_Elements(n_freq) EQ 0 THEN n_freq=psf.n_freq
        IF N_Elements(freq_i_use) EQ 0 THEN freq_i_use=findgen(n_freq)
        nf_use=N_Elements(freq_i_use)
        freq_bin_use=freq_bin_i[freq_i_use]
        fbin_use=freq_bin_use[Uniq(freq_bin_use,Sort(freq_bin_use))]
        nbin=N_Elements(Uniq(freq_bin_use,Sort(freq_bin_use)))

        IF keyword_set(beam_gaussian_params) THEN BEGIN
            FOR bin0=0L,nbin-1 DO BEGIN
                fbin=fbin_use[bin0]
                nf_bin=Float(Total(freq_bin_use EQ fbin))
                beam_single=FLTARR(dimension,elements)
                FOR gi=0,n_groups-1 DO BEGIN
                    ;(2.*obs.kpix) is the ratio of full sky (2 in l,m) to the analysis range (1/obs.kpix)
                    ;(2.*obs.kpix*dimension/psf.pix_horizon) is the scale factor between the psf pixels-to-horizon and the 
                    ; analysis pixels-to-horizon 
                    ;(0.5/obs.kpix) is the resolution scaling of what the beam model was made at and the current res 
                    beam_single+=gaussian_decomp(FINDGEN(dimension),FINDGEN(elements),$
                      (*psf.beam_gaussian_params[pol_i,gi_ref[gi]])[*,fbin],model_npix=psf.pix_horizon*1.3,$
                      model_res=(2.*obs.kpix*dimension)/psf.pix_horizon*(0.5/obs.kpix))*group_n[gi_use[gi]]
                ENDFOR
                beam_single/=Total(group_n[gi_use])
                beam_base+=nf_bin*beam_single*beam_single
                n_bin_use+=nf_bin*freq_norm[fbin]
            ENDFOR
        ENDIF ELSE BEGIN
            FOR bin0=0L,nbin-1 DO BEGIN
                fbin=fbin_use[bin0]
                nf_bin=Float(Total(freq_bin_use EQ fbin))
                beam_single=Complexarr(psf_dim,psf_dim)
                FOR gi=0,n_groups-1 DO BEGIN
                    beam_single+=Reform(*(*beam_arr[pol_i,fbin,gi_ref[gi]])[rbin,rbin]*group_n[gi_use[gi]],psf_dim,psf_dim)
                ENDFOR
                beam_single/=Total(group_n[gi_use])
                IF Keyword_Set(abs) THEN beam_single=Abs(beam_single)
                beam_base_uv1=Complexarr(dimension,elements)
                beam_base_uv1[xl:xh,yl:yh]=beam_single
                beam_base_single=fft_shift(FFT(fft_shift(beam_base_uv1),/inverse));/2.
            
                beam_base+=nf_bin*Real_part(beam_base_single*Conj(beam_base_single));>0
                n_bin_use+=nf_bin*freq_norm[fbin]
            ENDFOR
        ENDELSE
    ENDELSE
ENDIF ELSE BEGIN
    ;backwards compatibility
    IF N_Elements(freq_bin_i) EQ 0 THEN BEGIN
        n_freq_bin=psf.n_freq
        beam_base_uv=complexarr(psf_dim,psf_dim)
        FOR fi=0,n_freq_bin-1 DO BEGIN
            beam_single=Complexarr(psf_dim,psf_dim)
            FOR gi=0,n_groups-1 DO BEGIN
                beam_single+=Reform(*(*beam_arr[pol_i,fi,gi_ref[gi]])[rbin,rbin]*group_n[gi_use[gi]],psf_dim,psf_dim)
            ENDFOR
            beam_single/=Total(group_n[gi_use])
            beam_base_uv+=beam_single
            n_bin_use+=1.*freq_norm[fi]
        ENDFOR
    ENDIF ELSE BEGIN
        IF N_Elements(n_freq) EQ 0 THEN n_freq=N_Elements(freq_bin_i)
        IF N_Elements(freq_i_use) EQ 0 THEN freq_i_use=findgen(n_freq)
        nf_use=N_Elements(freq_i_use)

        IF keyword_set(beam_gaussian_params) THEN BEGIN
            beam_base=Fltarr(dimension,elements)
            FOR fi0=0L,nf_use-1 DO BEGIN
                fi=freq_i_use[fi0]
                IF N_Elements(freq_i) GT 0 THEN IF Total(freq_i EQ fi) EQ 0 THEN CONTINUE
                fbin=freq_bin_i[fi]
                beam_single=FLTARR(dimension,elements)
                FOR gi=0,n_groups-1 DO BEGIN
                    ;(2.*obs.kpix) is the ratio of full sky (2 in l,m) to the analysis range (1/obs.kpix)
                    ;(2.*obs.kpix*dimension/psf.pix_horizon) is the scale factor between the psf pixels-to-horizon and the 
                    ; analysis pixels-to-horizon
                    ;(0.5/obs.kpix) is the resolution scaling of what the beam model was made at and the current res 
                    beam_single+=gaussian_decomp(FINDGEN(dimension),FINDGEN(elements),$
                      (*psf.beam_gaussian_params[pol_i,gi_ref[gi]])[*,fbin],model_npix=psf.pix_horizon*1.3,$
                      model_res=(2.*obs.kpix*dimension)/psf.pix_horizon*(0.5/obs.kpix))*group_n[gi_use[gi]]
                ENDFOR
                beam_single/=Total(group_n[gi_use])
                beam_base+=beam_single
                n_bin_use+=1.*freq_norm[fbin]
            ENDFOR
        ENDIF ELSE BEGIN
            beam_base_uv=complexarr(psf_dim,psf_dim)
            FOR fi0=0L,nf_use-1 DO BEGIN
                fi=freq_i_use[fi0]
                IF N_Elements(freq_i) GT 0 THEN IF Total(freq_i EQ fi) EQ 0 THEN CONTINUE
                fbin=freq_bin_i[fi]
                beam_single=Complexarr(psf_dim,psf_dim)
                FOR gi=0,n_groups-1 DO BEGIN
                    beam_single+=Reform(*(*beam_arr[pol_i,fbin,gi_ref[gi]])[rbin,rbin]*group_n[gi_use[gi]],psf_dim,psf_dim)
                ENDFOR
                beam_single/=Total(group_n[gi_use])
                beam_base_uv+=beam_single
                n_bin_use+=1.*freq_norm[fbin]
            ENDFOR
        ENDELSE 
    ENDELSE
  
    IF ~keyword_set(beam_gaussian_params) THEN BEGIN 
        beam_base_uv1=Complexarr(dimension,elements)
        beam_base_uv1[xl:xh,yl:yh]=beam_base_uv
        beam_base=fft_shift(FFT(fft_shift(beam_base_uv1),/inverse));/2.
    ENDIF

ENDELSE
beam_base/=n_bin_use
beam_base=real_part(beam_base)

RETURN,beam_base
END
