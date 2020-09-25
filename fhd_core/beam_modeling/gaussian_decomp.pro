;;
;; Using input parameters p, build a gaussian mixture model of the image beam on 
;; the x,y grid. 
;;
;; x: Required x-axis vector
;; y: Required y-axis vector
;; p: Required gaussian parameter vector, 
;;  ordered as amp, offset x, sigma x, offset y, sigma y per lobe
;; fft: Optionally return the analytic Fourier Transform of the input gaussians
;; model_npix: Optionally provide the number of pixels on an axis used to derive
;;  the input parameters to convert to the current x,y grid 
;; model_res: Optionally provide the grid resolution used to derive the input
;;  parameters to convert to the current grid resolution
;; volume_beam: Returns the analytical calculation of the beam volume
;; sq_volume_beam: Returns the analytical calculation of the squared beam volume

function gaussian_decomp, x, y, p, fft=fft, model_npix=model_npix, model_res=model_res,$
  volume_beam=volume_beam,sq_volume_beam=sq_volume_beam
  
  nx = N_elements(x)
  ny = N_elements(y)
  decomp_beam=FLTARR(nx,ny)
  var = reform(p,5,N_elements(p)/5)  
  n_lobes = (size(var))[2]
  i = Complex(0,1)

  ;If the parameters were built on a different grid, then put on new grid
  ;Npix only affects the offset params
  ;Assumes model grid was smaller
  if keyword_set(model_npix) then begin
    offset = abs(nx/2. - model_npix/2.)
    var[1,*] = var[1,*] + offset 
    var[3,*] = var[3,*] + offset
  endif
  ;Res affects gaussian sigma and offsets
  if keyword_set(model_res) then begin
    var[2,*] = var[2,*] * model_res
    var[4,*] = var[4,*] * model_res
    var[1,*] = ((var[1,*]-nx/2.) * model_res) + nx/2.
    var[3,*] = ((var[3,*]-ny/2.) * model_res) + ny/2.
  endif
  
            
  if ~keyword_set(fft) then begin
    ;Full image model with all the gaussian components
    for lobe_i=0, n_lobes - 1 do begin
      decomp_beam += var[0,lobe_i] * $
        (exp(-(x-var[1,lobe_i])^2/(2*var[2,lobe_i]^2))#exp(-(y-var[3,lobe_i])^2/(2*var[4,lobe_i]^2)))  
    endfor
  endif else begin
    ;Full uv model with all the gaussian components
    volume_beam = total(var[0,*])
    sq_volume_beam = !Dpi*total((var[2,*])*(var[4,*])*var[0,*]^2)/(nx*ny)

    kx = (FINDGEN(nx)-nx/2.)#(FLTARR(ny)+1.)
    ky = (FLTARR(nx)+1.)#(FINDGEN(ny)-ny/2.)
    for lobe_i=0, n_lobes - 1 do begin 
      decomp_beam += var[0,lobe_i]*2.*!pi/(nx*ny)*var[2,lobe_i]*var[4,lobe_i]* $
        exp(-2*!pi^2/(nx*ny)*(var[2,lobe_i]^2*kx^2+var[4,lobe_i]^2*ky^2)-$
        2*!pi/(nx*ny)*i*(var[1,lobe_i]*kx+var[3,lobe_i]*ky))
    endfor
  endelse
  
  return, decomp_beam

end

