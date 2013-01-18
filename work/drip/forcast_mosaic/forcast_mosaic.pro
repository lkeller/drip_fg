pro forcast_mosaic,fileroot,exptime=exptime,shifts=shifts,rof=rof

pixscale=0.77   ;arcsec/pixel  

readcol,fileroot+'.txt',objfiles,maskfiles,format='A,A'

ims=fdrp_getdata(objfiles,headers=hdrs)
bps=fdrp_getdata(maskfiles)

imsize=size(ims)

exp=fltarr(imsize[3])

;if rof not specified, then set to 0
if not keyword_set(rof) then begin
  rof=fltarr(imsize[3])
  rof[*]=0.0
endif

;exposure not specified, then get exposure time from headers
if not keyword_set(exptime) then begin  

  j=0
  for j=0,imsize[3]-1 do begin 
    hdr=strarr(500)
    hdr[*]=hdrs[*,j]
    exptime=sxpar(hdr,'EXPTIME')  ;in seconds
    exp[j]=exptime
    endfor  

endif else begin 
  exp[*]=exptime 
endelse

;if shifts not specified, determine shifts from RA/Dec in headers
if not keyword_set(shifts) then begin  
  shifts=fltarr(2,imsize[3])
  shifts[*,0]=[0,0]

  ra_0=15*sxpar(hdrs[*,0],'TELRA')
  dec_0=sxpar(hdrs[*,0],'TELDEC')
  print,ra_0,dec_0

  i=1
  for i=1,imsize[3]-1 do begin 
    hdr=strarr(500)
    hdr[*]=hdrs[*,i]
    ra=15*sxpar(hdr,'TELRA')
    dec=sxpar(hdr,'TELDEC')
    print,ra,dec
    shifts[*,i]=[-ra+ra_0,dec-dec_0]
    endfor  

  shifts=shifts*3600  ;convert to arcsec
  shifts=shifts/pixscale  ;convert to pixels
endif

print,shifts
print,rof
print,exptime

fdrp_mosaicims,ims,exp,bps,mos,expmap,wtmap,shifts_in=shifts,rot_angle=rof
atv,mos/expmap
writefits,fileroot+'.fits',mos/expmap

end
