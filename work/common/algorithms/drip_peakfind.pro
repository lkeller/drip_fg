;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Name: PEAKFIND.PRO
; Author: WDV
; Date: 15 Jul 2011
; Purpose: To find peaks (stars) in FORCAST images
; Inputs:
;	ifile	- input FITS file or image array
; Outputs:
;       Coordinates of objects found are printed to screen or dumped to output file if OUTFILE keyword
;		is specified
; Keywords:
;	XARR   - output array of x coordinates
;	YARR   - output array of y coordinates
; 	NPEAKS - number of peaks to find, usually determined from observing mode (INSTMODE) in FITS header
;	FWHM   - fwhm in pixels
;       THRESH - sigma level above background to identify real objects, default = 5
;       TSTEP  - step in sgima level to adjust THRESH in order to find NPEAKS in the image
;       SHARPLIM - 2 element vector giving lower and upper limits for sharpness parameter; default = [0.2,1.0]]
;       ROUNDLIM - 2 element vector giving lower and upper limits for roundness parameter; default = [-0.75,0.75]
;       OUTFILE - name of output file
;       IMGARRAY - set if input is an image array rather than a FITS file
;       BCKFIT  - set to subtract the background first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;****************************************************************************
;     SORTPEAKS - select peaks that follow a particular pattern for merge
;****************************************************************************
FUNCTION sortpeaks, x, y,chopdist, noddist,epsilon
  
  resarr = [-1]
  for i=0,n_elements(x)-1 do begin
    dist = sqrt((x[i]-x)^2+(y[i]-y)^2)
    deltachop = abs(dist-chopdist)
    deltanod = abs(dist-noddist)
    deltachopnod = abs(dist-sqrt(chopdist^2+noddist^2))

    kchop = where(deltachop lt epsilon)
    knod = where(deltanod lt epsilon)
    kchopnod = where(deltachopnod lt epsilon)
    
    Nmatch = 0
    if kchop(0) ne -1 then Nmatch = Nmatch+n_elements(kchop)
    if knod(0) ne -1 then Nmatch = Nmatch+n_elements(knod)
    if kchopnod(0) ne -1 then Nmatch = Nmatch+n_elements(kchopnod)

    if (Nmatch ge 2) then resarr = [resarr,i]
  endfor
  
  if (n_elements(resarr) eq 1) then return,[-1,-1]
  
  final_index = resarr(1:n_elements(resarr)-1)

  return,[[x[final_index]],[y[final_index]]]
END 


;****************************************************************************
;     DRIP_PEAKFIND - find peaks in an image
;****************************************************************************
function drip_peakfind, coadded, newimage, XARR=xarr, YARR=yarr, $
                      NPEAKS=npeaks, FWHM=fwhm, THRESH=thresh, TSTEP=tstep, $
		      SHARPLIM=sharplim, ROUNDLIM=roundlim, OUTFILE=outfile, SILENT=silent, BCKFIT=bckfit, $
		      STARS=stars, CHOPNODDIST=chopnoddist,showatv=showatv

forward_function backfit

print,'N_PARAMS =',N_PARAMS()

if not(keyword_set(THRESH)) then thresh = 5.0
if not(keyword_set(FWHM))   then fwhm   = 4.5
if not(keyword_set(TSTEP))  then tstep  = 0.25
if not(keyword_set(NPEAKS)) then npeaks = 4
if not(keyword_set(SHARPLIM)) then sharplim = [0.2,1.0]
if not(keyword_set(ROUNDLIM)) then roundlim = [-0.75,0.75]


; this is used when checking with sortpeaks if the 
; return values are coherent with chopnoddist
epsilon = 5
initthesh = thresh
inittstep = tstep

; maximum number of iterations finding loops
maxloop = 1000

; Create a psf function to be convolved with the images
psf = psf_gaussian(NPIX=50,FWHM=fwhm,/NORMALIZE)

; If the array newimage is input then we calculate the
; peaks for that image. If not we set x and y to 0.
; if new image is input then we will return the shift
; else we return the peak positions
if N_PARAMS() eq 2 then begin

  img          = newimage
  if keyword_set(BCKFIT) then begin
     imfit     = backfit(img,NBLCKS=15)
  endif else begin
     imfit     = img
  endelse 
  img_conv     = convolve(imfit, psf)
  img_conv_abs = abs(img_conv)
  bkgd         = median(img_conv_abs)
  res          = moment(img_conv_abs)
  sigma        = sqrt(res[1])
  cutlev       = thresh*sigma


  nfound = 0
  nloop  = 0
  while (nfound lt npeaks) do begin
	 find, img_conv_abs, x, y, f, sharp, rnd, cutlev, fwhm, roundlim, sharplim, /SILENT 
	 nfound = n_elements(x)
	 thresh = thresh - tstep
	 cutlev = thresh*sigma
	 nloop  = nloop + 1
	 if nloop gt 1000 then goto, outloop
  end

  ; since nfound starts at 0, only gets here after lowering thresh until nfound > npeaks

  while (nfound ne npeaks) do begin
	 tstep = tstep/2.0
	 find, img_conv_abs, x, y, f, sharp, rnd, cutlev, fwhm, roundlim, sharplim, /SILENT 
	 nfound = n_elements(x)
	 if (nfound gt npeaks) then thresh = thresh + tstep else thresh = thresh - tstep
	 cutlev = thresh*sigma
	 nloop  = nloop + 1
	 if nloop gt 1000 then goto, outloop
  end
  
  ; This is the other algorithm that I implemented but does not seem to work
  ;threshhigh = 1
  ;currentstep = tstep
  ;debugstep2 = [tstep]
  ;debugthrest2 = [c_thresh]
  ;while (nfound ne npeaks) and (nloop le maxloop) do begin  ;(nfound lt npeaks) do begin
  ;	 find, img_conv_abs, x, y, f, sharp, round, cutlev, fwhm, roundlim, sharplim, /SILENT 
  ;	 nfound = n_elements(x)
  ;	 if nfound lt npeaks then begin
  ;	   if threshhigh eq 0 then currentstep = currentstep/2.
  ;	   threshhigh = 1
  ;	   thresh = thresh - currentstep 
  ;	  endif else begin 
  ;	    if threshhigh eq 1 then currentstep = currentstep/2.
  ;	    threshhigh = 0
  ;	    thresh = thresh + currentstep
  ;	  endelse
  ;	  ;debugstep2 = [debugstep2,currentstep]
  ;	  ;debugthrest2 = [debugthrest2,thresh]
  ;	  cutlev = thresh*sigma
  ;	  nloop  = nloop + 1
  ;end
  
  ;window,0,retain=2
  ;plot,debugstep2
  ;window,1,retain=2
  ;plot,debugthrest2
  
  if (nloop gt maxloop) then begin
    print, 'Number of loops run   = ',nloop
    print, 'Peaks found = '+strtrim(nfound,2)+'/'+strtrim(npeaks,2)
    print, 'Threshold used = '+strtrim(thresh,2)+'/'+strtrim(initthesh,2)
    if N_PARAMS() eq 2 then begin 
      print, 'Returning default value: shift = [0.,0.]'
      return, [replicate(0.,npeaks),replicate(0.,npeaks)]
    endif else begin
      return, [replicate(10.*fwhm,npeaks),replicate(10.*fwhm,npeaks)]
    endelse
  endif

  outloop:
  if keyword_set(chopnoddist) then begin
    if n_elements(chopnoddist) eq 2 then begin
      xysorted = sortpeaks(x, y, chopnoddist[0], chopnoddist[1], epsilon)
      if xysorted(0,0) ne -1 then begin
	x = xysorted(*,0)
	y = xysorted(*,1)
	nfound = n_elements(x)
      endif
    endif
  endif
  
  if not(keyword_set(SILENT)) then begin
    print, 'Number of loops run   = ',nloop
    print, 'Number of peaks found = ',nfound
    print, '      X            Y           F           Sharp         Round'

    for j=0,nfound-1 do begin
	print, x[j],y[j],f[j],sharp[j],rnd[j]
    endfor
  endif
  
endif else begin
  x=0.
  y=0.
endelse


coadd        = coadded
if keyword_set(BCKFIT) then begin
   coaddfit     = backfit(coadd,NBLCKS=15)
endif else begin
   coaddfit     = coadd
endelse

c_thresh       = initthesh
c_tstep        = inittstep

coadd_conv     = convolve(coaddfit, psf)
coadd_conv_abs = abs(coadd_conv)
c_bkgd         = median(coadd_conv_abs)
c_res          = moment(coadd_conv_abs)
c_sigma        = sqrt(c_res[1])
c_cutlev       = c_thresh*c_sigma

; Calculate peaks for input array coadded
nfound = 0
nloop  = 0
while (nfound lt npeaks) do begin
       find, coadd_conv_abs, cx, cy, f, sharp, rnd, c_cutlev, fwhm, roundlim, sharplim, /SILENT 
       nfound = n_elements(cx)
       c_thresh = c_thresh - c_tstep
       c_cutlev = c_thresh*c_sigma
       nloop  = nloop + 1
       if nloop gt 1000 then goto, c_outloop
end

; since nfound starts at 0, only gets here after lowering thresh until nfound > npeaks

while (nfound ne npeaks) do begin
       c_tstep = c_tstep/2.0
       find, coadd_conv_abs, cx, cy, f, sharp, rnd, c_cutlev, fwhm, roundlim, sharplim, /SILENT 
       nfound = n_elements(cx)
       if (nfound gt npeaks) then c_thresh = c_thresh + c_tstep else c_thresh = c_thresh - c_tstep
       c_cutlev = c_thresh*c_sigma
       nloop  = nloop + 1
       if nloop gt 1000 then goto, c_outloop
end

c_outloop:

; This is the other algorithm that I implemented but does not seem to work
;threshhigh = 1
;currentstep = tstep
;debugstep1 = [tstep]
;debugthrest1 = [c_thresh]
;while (nfound ne npeaks) and (nloop le maxloop) do begin  ;(nfound lt npeaks and nfound gt npeaks) do begin
;       find, coadd_conv_abs, cx, cy, f, sharp, round, c_cutlev, fwhm, roundlim, sharplim, /SILENT
;       nfound = n_elements(cx)
;	if nfound lt npeaks then begin
;	  if threshhigh eq 0 then currentstep = currentstep/2.
;	  threshhigh = 1
;	  c_thresh = c_thresh - currentstep 
;	endif else begin 
;	  if threshhigh eq 1 then currentstep = currentstep/2.
;	  threshhigh = 0
;	  c_thresh = c_thresh + currentstep
;	endelse
;	;debugstep1 = [debugstep1,currentstep]
;	;debugthrest1 = [debugthrest1,c_thresh]
;	c_cutlev = c_thresh*c_sigma
;	nloop  = nloop + 1
;end
  
  ;window,2,retain=2
  ;plot,debugstep1
  ;window,3,retain=2
  ;plot,debugthrest1
  
if (nloop gt maxloop) then begin
  print, 'Number of loops run	= ',nloop
  print, 'Peaks found = '+strtrim(nfound,2)+'/'+strtrim(npeaks,2)
  print, 'Threshold used = '+strtrim(c_thresh,2)+'/'+strtrim(initthesh,2)
  if N_PARAMS() eq 2 then begin 
    print, 'Returning default value: shift = [0.,0.]'
    return, [replicate(0.,npeaks),replicate(0.,npeaks)]
  endif else begin
    return, [replicate(10.*fwhm,npeaks),replicate(10.*fwhm,npeaks)]
  endelse
endif

if keyword_set(chopnoddist) then begin
  if n_elements(chopnoddist) eq 2 then begin
    xysorted = sortpeaks(cx, cy, chopnoddist[0], chopnoddist[1], epsilon)
    if xysorted(0,0) ne -1 then begin
      cx = xysorted(*,0)
      cy = xysorted(*,1)
      nfound = n_elements(cx)
    endif
  endif
endif

; For coadd we should match peaks from x,y and cx,cy

;if keyword_set(showatv) then begin
;  atv22, coadded
;  for j=0,nfound-1 do $
;    atvplot, cx[j],cy[j], psym=4
;endif

if not(keyword_set(SILENT)) then begin
  print, 'Number of loops run   = ',nloop
  print, 'Number of peaks found = ',nfound
  print, '      X            Y           F           Sharp         Round'

  for j=0,nfound-1 do begin
      print, cx[j],cy[j],f[j],sharp[j],rnd[j]
  endfor
endif

if keyword_set(STARS) then begin
  stars=transpose([[cx],[cy]])
endif

;shift_coords = coords[1,*]-coords[0,*]
shift_coordsx = cx - x 
shift_coordsy = cy - y 
return,[[shift_coordsx],[shift_coordsy]]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function backfit, im, NBLCKS=nblcks, SMSIZE=smsize

if not(keyword_set(NBLCKS)) then nblcks = 10
if not(keyword_set(SMSIZE)) then smsize = 10

; Strip off the edges

s  = size(im)
nx = s[1]
ny = s[2]

im[0,*]    = 0.
im[nx-1,*] = 0.
im[*,0]    = 0.
im[*,ny-1] = 0.

; Fit the background

imfilt = fiterpolate(im,nblcks,nblcks)

; Smooth the result

imsmoo = smooth(imfilt,smsize)

; Subtract the background

imout  = im - imsmoo
bg     = imsmoo

;atv, imout
;writefits, outfile, imout, h

return, imout

end 
