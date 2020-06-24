pro mwa_beam_gaussian_decomp, cen, pix_hor, parinfo=parinfo, parvalues=p, freq=freq, pol=pol, $
  gauss_beam_fbin = gauss_beam_fbin
  ;ordered by amp, offset x, sigma x, offset y, sigma y per lobe

  ;Input gaussian parameters to help convergence
  ;pr: primary lobe, sw1: first west sidelobe, sw2: second west sidelobe
  ;se1: first east sidelobe, se2: second east sidelobe, sn1: first north sidelobe
  ;sn2: second north sidelobe, ss1: first south sidelobe, ss2: second sourth sidelobe
  ;sne1: first north-east sidelobe, snw1: first north-west sidelobe, 
  ;sse1: first south-east sidelobe, ssw1: first south-west sidelobe
  p = [$
    1.00,cen,pix_hor/14.0,cen,pix_hor/13.1,$ ;pr
    -0.065,cen,pix_hor/14.9,cen-pix_hor/6.4,pix_hor/23.1,$ ;pr s null
    -0.065,cen,pix_hor/14.9,cen+pix_hor/6.4,pix_hor/23.1,$ ;pr n null
    -0.065,cen-pix_hor/6.8,pix_hor/22.9,cen,pix_hor/14.0,$ ;pr w null
    -0.065,cen+pix_hor/6.8,pix_hor/22.9,cen,pix_hor/14.0,$ ;pr e null
    0.05,cen-pix_hor/3.65,pix_hor/24.3,cen,pix_hor/14.5,$ ;sw1
    0.007,cen-pix_hor/2.26,pix_hor/37.4,cen,pix_hor/15.2,$ ;sw2
    0.05,cen+pix_hor/3.65,pix_hor/24.5,cen,pix_hor/14.5,$ ;se1
    0.007,cen+pix_hor/2.26,pix_hor/37.9,cen,pix_hor/15.2,$;se2
    0.06,cen,pix_hor/15.2,cen+pix_hor/3.65,pix_hor/25.5,$ ;sn1
    0.021,cen,pix_hor/16.1,cen+pix_hor/2.3,pix_hor/37.6,$ ;sn2
    0.06,cen,pix_hor/15.2,cen-pix_hor/3.65,pix_hor/25.5,$ ;ss1
    0.021,cen,pix_hor/16.1,cen-pix_hor/2.3,pix_hor/37.6,$ ;ss2
    0.003,cen+pix_hor/3.75,pix_hor/31.8,cen+pix_hor/3.65,pix_hor/31,$ ;sne1
    0.003,cen-pix_hor/3.8,pix_hor/31.6,cen+pix_hor/3.65,pix_hor/31,$ ;snw1
    0.003,cen+pix_hor/3.75,pix_hor/31.8,cen-pix_hor/3.65,pix_hor/31,$ ;sse1
    0.003,cen-pix_hor/3.8,pix_hor/31.6,cen-pix_hor/3.65,pix_hor/31];ssw1

  ;Model was made at high-band center frequency, scale appropriately
  freq_dep = 1.82435e8 / freq
  var = reform(p,5,N_elements(p)/5.)
  var[1,*] = ((cen - var[1,*]) * freq_dep) + cen
  var[3,*] = ((cen - var[3,*]) * freq_dep) + cen
  var[2,*] *= freq_dep
  var[4,*] *= freq_dep
  ;var[2,1:3] /= freq_dep ;remove freq dependence of width of primary nulls
  ;var[4,1:3] /= freq_dep

  ;Model was made in x polarization, flip appropriately
  if pol EQ 1 then begin
    temp = var[1,*]
    var[1,*] = var[3,*]
    var[3,*] = temp
    temp = var[2,*]
    var[2,*] = var[4,*]
    var[4,*] = temp
  endif

  p = reform(var,N_elements(p))


  parinfo = replicate({value:0.,fixed:0,tied:''},N_elements(p))
  parinfo[*].value = p ; just for record keeping purposes...cannot be used with the .fixed input (self-referencing issues)

  ;Fix amplitude parameters together which are always in the image fully
  parinfo[0].fixed = 1
  parinfo[5].tied = 'p[10]' ;pr s null amp = pr n null amp
  parinfo[15].tied = 'p[20]' ;pr w null amp = pr e null amp
  parinfo[25].tied = 'p[35]' ;sw1 amp = se1 amp
  parinfo[30].tied = 'p[40]' ;sw2 amp = se2 amp
  parinfo[45].tied = 'p[55]' ;sn1 amp = ss1 amp
  parinfo[50].tied = 'p[60]' ;sn2 amp = ss2 amp

  parinfo[9].tied = 'p[14]' ;pr s null sigma y = pr n null sigma y
  parinfo[19].tied = 'p[24]' ;pr w null sigma y = pr e null sigma y
  parinfo[7].tied = 'p[12]' ;pr s null sigma x = pr n null sigma x
  parinfo[17].tied = 'p[22]' ;pr w null sigma x = pr e null sigma x

  ;Default number of points for the linear least-squares fitter for the MWA
  gauss_beam_fbin = 16.  

end
