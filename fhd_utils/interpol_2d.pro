FUNCTION interpol_2d,image, mask
; Interpolate a 2D image, filling NAN values and/or values at masked locations
dimension = (size(image, /dimension))[0]
elements = (size(image, /dimension))[1]
 
result_x = fltarr(dimension, elements)
weights_x = fltarr(dimension, elements)
result_y = fltarr(dimension, elements)
weights_y = fltarr(dimension, elements)
image_use = image
IF N_Elements(mask) GT 0 THEN BEGIN
    i_cut = where(mask EQ 0, n_cut)
    IF n_cut GT 0 THEN image_use[i_cut] = !Values.F_NAN
    i_use = where(Finite(image_use), n_use)
ENDIF ELSE BEGIN
    mask = fltarr(dimension, elements)
    i_use = where(Finite(image_use), n_use)
    mask[i_use] = 1
ENDELSE

mask_i_threshold = dimension/2
FOR i=0, dimension-1 DO BEGIN
    mask_sum = Total(mask[i, *])
    IF mask_sum LE mask_i_threshold THEN CONTINUE
    result_x[i, *] = interpol(image_use[i, *], dimension, /NAN)
    weights_x[i, *] = mask_sum
ENDFOR
FOR j=0, elements-1 DO BEGIN
    mask_sum = Total(mask[*, j])
    IF mask_sum LE mask_i_threshold THEN CONTINUE
    result_y[*, j] = interpol(image_use[*, j], elements, /NAN)
    weights_y[*, j] = mask_sum
ENDFOR
;i_cut = where((weights_x + weights_y) 
result = (result_x*weights_x + result_y*weights_y) * weight_invert(weights_x + weights_y)
RETURN,result
END