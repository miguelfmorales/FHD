pro mwa_beam_gaussian_decomp, cen, pix_hor, obs, parinfo=parinfo, parvalues=p, freq=freq, pol=pol, $
  gauss_beam_fbin = gauss_beam_fbin

  ;scale gaussians by the resolution factor the the model was made with
  res_scale = 0.5 / obs.kpix
  pix_hor = pix_hor / res_scale

  ;Find the pointing to get the correct gaussian decomp default
  delays = *obs.delays
  if total(abs(delays - (FINDGEN(16) mod 4)*2)) EQ 0 then pointing=-2
  if total(abs(delays - (FINDGEN(16) mod 4)*1)) EQ 0 then pointing=-1
  if total(abs(delays - (FINDGEN(16) mod 4)*0)) EQ 0 then pointing=0
  if total(abs(delays - (reverse(FINDGEN(16)) mod 4)*1)) EQ 0 then pointing=1
  if total(abs(delays - (reverse(FINDGEN(16)) mod 4)*2)) EQ 0 then pointing=2
  if N_elements(pointing) EQ 0 then begin
    print, 'Gaussian decomp does not have this pointing default, fitter may struggle'
    pointing=0
  endif
    
  ;Input gaussian parameters to help convergence
  ;pr: primary lobe, sw1: first west sidelobe, sw2: second west sidelobe
  ;se1: first east sidelobe, se2: second east sidelobe, sn1: first north sidelobe
  ;sn2: second north sidelobe, ss1: first south sidelobe, ss2: second sourth sidelobe
  ;sne1: first north-east sidelobe, snw1: first north-west sidelobe, 
  ;sse1: first south-east sidelobe, ssw1: first south-west sidelobe
  ;ordered by amp, offset x, sigma x, offset y, sigma y per lobe
  if pointing EQ 0 then begin
    p = [$
      1.00,cen,pix_hor/14.0,cen,pix_hor/13.1,$ ;pr
      -0.065,cen,pix_hor/14.9,cen-pix_hor/6.4,pix_hor/23.1,$ ;pr s null
      -0.065,cen,pix_hor/14.9,cen+pix_hor/6.4,pix_hor/23.1,$ ;pr n null
      -0.065,cen-pix_hor/6.8,pix_hor/22.9,cen,pix_hor/14.0,$ ;pr w null
      -0.065,cen+pix_hor/6.8,pix_hor/22.9,cen,pix_hor/14.0,$ ;pr e null
      0.06,cen,pix_hor/15.2,cen+pix_hor/3.65,pix_hor/25.5,$ ;sn1
      0.021,cen,pix_hor/16.1,cen+pix_hor/2.3,pix_hor/37.6,$ ;sn2
      0.06,cen,pix_hor/15.2,cen-pix_hor/3.65,pix_hor/25.5,$ ;ss1
      0.021,cen,pix_hor/16.1,cen-pix_hor/2.3,pix_hor/37.6,$ ;ss2
      0.05,cen-pix_hor/3.65,pix_hor/24.3,cen,pix_hor/14.5,$ ;sw1
      0.007,cen-pix_hor/2.26,pix_hor/37.4,cen,pix_hor/15.2,$ ;sw2
      0.05,cen+pix_hor/3.65,pix_hor/24.5,cen,pix_hor/14.5,$ ;se1
      0.007,cen+pix_hor/2.26,pix_hor/37.9,cen,pix_hor/15.2,$;se2
      0.003,cen+pix_hor/3.75,pix_hor/31.8,cen+pix_hor/3.65,pix_hor/31,$ ;sne1
      0.003,cen-pix_hor/3.8,pix_hor/31.6,cen+pix_hor/3.65,pix_hor/31,$ ;snw1
      0.003,cen+pix_hor/3.75,pix_hor/31.8,cen-pix_hor/3.65,pix_hor/31,$ ;sse1
      0.003,cen-pix_hor/3.8,pix_hor/31.6,cen-pix_hor/3.65,pix_hor/31];ssw1
  endif
  if (pointing EQ -1) OR (pointing EQ 1) then begin
    ;; Params are for pointing -1. However, this can also be the params for 
    ;;  pointing 1 if they are flipped about the x-axis below
    p = [$
      1.00,cen,pix_hor/13.3,cen,pix_hor/13.2,$ ;pr
      -0.07,cen,pix_hor/14.4,cen-pix_hor/6.45,pix_hor/22.1,$ ;pr s null
      -0.07,cen,pix_hor/14.4,cen+pix_hor/6.47,pix_hor/22.1,$ ;pr n null
      -0.1,cen-pix_hor/7.4,pix_hor/18,cen,pix_hor/13.8,$ ;pr w null
      -0.1,cen+pix_hor/6.2,pix_hor/18,cen,pix_hor/13.8,$ ;pr e null
      0.066,cen-pix_hor/60.8,pix_hor/15.2,cen+pix_hor/3.7,pix_hor/25,$ ;sn1
      0.021,cen-pix_hor/29.3,pix_hor/16.2,cen+pix_hor/2.3,pix_hor/38.5,$ ;sn2
      0.066,cen+pix_hor/63.5,pix_hor/15.2,cen-pix_hor/3.7,pix_hor/24.9,$ ;ss1
      0.021,cen+pix_hor/61.6,pix_hor/16.5,cen-pix_hor/2.3,pix_hor/38,$ ;ss2
      0.067,cen-pix_hor/3.89,pix_hor/22.5,cen-pix_hor/64,pix_hor/14.4,$ ;sw1
      0.017,cen-pix_hor/2.2,pix_hor/29.1,cen-pix_hor/38,pix_hor/14.8,$ ;sw2
      0.067,cen+pix_hor/4.5,pix_hor/17.4,cen+pix_hor/72,pix_hor/14.1,$ ;se1
      0.001,cen+pix_hor/2.37,pix_hor/93.4,cen+pix_hor/46,pix_hor/18.5,$;se2
      0.002,cen+pix_hor/4.0,pix_hor/34.75,cen+pix_hor/3.5,pix_hor/32.9,$ ;sne1
      0.004,cen-pix_hor/3.55,pix_hor/30.3,cen+pix_hor/3.9,pix_hor/30.6,$ ;snw1
      0.002,cen+pix_hor/3.57,pix_hor/34.6,cen-pix_hor/3.9,pix_hor/32.7,$ ;sse1
      0.004,cen-pix_hor/4.0,pix_hor/30.4,cen-pix_hor/3.4,pix_hor/30.6];ssw1
  endif
  if (pointing EQ -2) OR (pointing EQ 2) then begin
    ;; Params are for pointing -2. However, this can also be the params for 
    ;;  pointing 2 if they are flipped about the x-axis below
    p = [$
      1.00,cen,pix_hor/12.6,cen,pix_hor/12.6,$ ;pr
      -0.1,cen,pix_hor/13.9,cen+pix_hor/6.8,pix_hor/16,$ ;pr s null
      -0.1,cen,pix_hor/13.9,cen-pix_hor/6.72,pix_hor/16,$ ;pr n null
      -0.14,cen+pix_hor/6.3,pix_hor/15.5,cen,pix_hor/13.4,$ ;pr w null
      -0.14,cen-pix_hor/7.3,pix_hor/15.5,cen,pix_hor/13.4,$ ;pr e null
      0.078,cen+pix_hor/37,pix_hor/14.8,cen-pix_hor/3.87,pix_hor/22,$ ;ss1 part2
      0.018,cen+pix_hor/35,pix_hor/17.9,cen-pix_hor/2.3,pix_hor/39.7,$ ;ss2
      0.078,cen-pix_hor/30,pix_hor/14.7,cen+pix_hor/3.88,pix_hor/22.3,$ ;sn1 part2
      0.018,cen-pix_hor/13.8,pix_hor/16.6,cen+pix_hor/2.3,pix_hor/41.6,$ ;sn2
      ;0.007, cen+pix_hor/3.1,pix_hor/5170,cen-pix_hor/19.5,  pix_hor/21.8,$ ;se1 part2
      0.09,  cen+pix_hor/4.8, pix_hor/16.9,cen+pix_hor/45,  pix_hor/13.6,$ ;se1 part2
      ;se2
      0.086, cen-pix_hor/3.89,pix_hor/20.6,cen-pix_hor/33,  pix_hor/14.2,$ ;sw1 part2
      0.031, cen-pix_hor/2.14,pix_hor/26.9,cen-pix_hor/18.1,pix_hor/14.9,$ ;sw2
      0.005, cen-pix_hor/4.2, pix_hor/29.3,cen-pix_hor/3.26,pix_hor/30.4,$ ;sse1
      0.005, cen-pix_hor/3.3, pix_hor/29.3,cen+pix_hor/4.13,pix_hor/30.6] ;snw1
  endif
  
  ;Model was made at high-band center frequency, scale appropriately
  freq_dep = 1.82435e8 / freq
  var = reform(p,5,N_elements(p)/5.)
  var[1,*] = ((var[1,*] - cen) * freq_dep) + cen
  var[3,*] = ((var[3,*] - cen) * freq_dep) + cen
  var[2,*] *= freq_dep
  var[4,*] *= freq_dep

  ;; Flip pointings 1,2 about the x-axis so that the -1,-2 parameters can be used above
  if (pointing EQ 2) OR (pointing EQ 1) then begin
    var[1,*] = (cen-var[1,*])+cen 
  endif

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
  parinfo[25].tied = 'p[35]' ;sn1 amp = ss1 amp
  parinfo[30].tied = 'p[40]' ;sn2 amp = ss2 amp
  if pointing EQ 0 then parinfo[45].tied = 'p[55]' ;sw1 amp = se1 amp
  if pointing EQ 0 then parinfo[50].tied = 'p[60]' ;sw2 amp = se2 amp

  parinfo[9].tied = 'p[14]' ;pr s null sigma y = pr n null sigma y
  parinfo[19].tied = 'p[24]' ;pr w null sigma y = pr e null sigma y
  parinfo[7].tied = 'p[12]' ;pr s null sigma x = pr n null sigma x
  parinfo[17].tied = 'p[22]' ;pr w null sigma x = pr e null sigma x

  ;Default number of points for the linear least-squares fitter for the MWA
  gauss_beam_fbin = 16.  

end
