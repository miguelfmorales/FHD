FUNCTION fast_dft,x_vec,y_vec,dimension=dimension,elements=elements,degpix=degpix,flux_arr=flux_arr,$
    dft_threshold=dft_threshold,conserve_memory=conserve_memory,return_kernel=return_kernel,_Extra=extra

IF N_Elements(elements) EQ 0 THEN elements=dimension

IF size(flux_arr,/type) EQ 10 THEN BEGIN
    n_pol=Total(Ptr_valid(flux_arr)) 
    flux_arr_use=flux_arr
    mem_free=0
ENDIF ELSE BEGIN
    n_pol=1
    flux_arr_use=Ptr_new(flux_arr)
    mem_free=1
ENDELSE 

IF N_Elements(model_uv_full) LT n_pol THEN model_uv_full=Ptrarr(n_pol)
IF Min(Ptr_valid(model_uv_full[0:n_pol-1])) EQ 0 THEN BEGIN
    FOR pol_i=0,n_pol-1 DO model_uv_full[pol_i]=Ptr_new(Complexarr(dimension,elements))
ENDIF
model_img=fast_dft_subroutine(x_vec,y_vec,flux_arr_use,dft_threshold=dft_threshold,$
    dimension=dimension,elements=elements,conserve_memory=conserve_memory,return_kernel=return_kernel)
FOR pol_i=0,n_pol-1 DO BEGIN
    model_uv=fft_shift(FFT(fft_shift(*model_img[pol_i]),/inverse)) ; normalization ??!!??!!
    *model_uv_full[pol_i]+=model_uv
ENDFOR

IF Keyword_Set(mem_free) THEN Ptr_free,flux_arr_use
Ptr_free,model_img
RETURN,model_uv_full
END