pro forcast_wcs,hdr

delta=-175.364
pos_0 = [242.73,395.57]

;get params from header
telrof=sxpar(hdr,'TELROF')
telra=15.0*sxpar(hdr,'TELRA')
teldec=sxpar(hdr,'TELDEC')

rof=telrof+delta

help,rof
help,telra
help,teldec
print,string(telra)


cdelt=0.75/3600.  ;deg. per pixel
rof=rof*!pi/180.

cd=[[-cdelt*cos(rof),-cdelt*sin(rof)],[-cdelt*sin(rof),cdelt*cos(rof)]]

MAKE_ASTR, astr, CD = cd, CRPIX = pos_0, CRVAL = [telra,teldec]
putast,hdr,astr

end