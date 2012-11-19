PRO spplot,in_array,xtitle=xtitle,ytitle=ytitle,xrange=xrange,over=over,order=order,rj=rj,flam=flam,nufnu=nufnu,_extra=e,fcol=fcol,nodata=nodata

; Written by Greg Sloan (Cornell University) for plotting spectral
; data in ascii columns: wavelength, flux, flux_error, spectral_order
;
;  1 Jun 06 added nufnu and lamflam keywords, both plot in lam F_lam units
; 18 May 06 checking p.font
; 25 Nov 05 can handle xrange=[0,0]
; 20 Sep 05 now treating /nodata separate from _extra
; 10 Aug 05 use only finite data when determining plot range
; 16 Jan 05 added fcol keyword
; 23 Dec 04 added flam keyword, plots in F_lam units
; 15 Dec 04 repaired a new bug I created 3 days ago with the over flag
; 12 Dec 04 major rewrite which simplifies the code and squashes the yrange bug
; 25 Jun 04 repaired major xrange bug
; 18 May 04 added keyword rj, plots Rayleigh-Jeans tail in lam^2*F_nu units
;  1 Feb 04 added keyword order, to plot one order
; 28 Dec 03 repaired minor bug with xrange for unsegmented plots
; 20 Dec 03 clearing up some bugs
; 15 Dec 03 modified to plot spectral segments separately
; 28 Nov 03 modified to add overplot capability
; 10 Nov 03 created
;
; plots the second column of data_array as a f'n of the first
; passes additional keywords on to plot
; sets default titles for x and y axis

lcol=0
if (n_elements(fcol) eq 0) then fcol=1
ocol=3

; check keywords - order and xrange require that the input array be trimmed

if (keyword_set(order) eq 0) then data_array=in_array else begin
  idx=where(in_array[ocol,*] eq order)
  if (max(idx) gt -1) then data_array=in_array[*,idx] else begin
    print,'Error in spplot.  No data of order ',order
    stop
  endelse
endelse
if (keyword_set(xrange) eq 0) then begin
  test_array=data_array
  xrange=[min(data_array[lcol,*]),max(data_array[lcol,*])]
endif else begin
  if (xrange[0] eq xrange[1]) then begin
    test_array=data_array
    xrange=[min(data_array[lcol,*]),max(data_array[lcol,*])]
  endif else begin
    xmin=min(xrange) & xmax=max(xrange)
    idx=where(data_array[lcol,*] ge xmin and data_array[lcol,*] le xmax)
    if (max(idx) gt -1) then test_array=data_array[*,idx] else begin
      print,'Error in spplot.  No data in range  ',xrange
      stop
    endelse
  endelse
endelse

; check other keywords and set xtitle and ytitle

if (keyword_set(over) eq 0) then overflag=0 else overflag=1
if (keyword_set(rj) eq 0) then rjflag=0 else rjflag=1
if (keyword_set(flam) eq 0) then flamflag=0 else flamflag=1
if (keyword_set(nufnu) eq 0 and keyword_set(lamflam) eq 0) then $
  lamflag=0 else lamflag=1
if (keyword_set(nodata) eq 0) then dataflag=1 else dataflag=0
if (keyword_set(xtitle) eq 0) then begin
  if (!p.font eq 1) then xtt='!9l!4 (!9m!4m)' $
    else xtt='!7k!5 (!7l!5m)' 
endif else xtt=xtitle
if (keyword_set(ytitle) eq 0) then begin
  if (rjflag eq 1) then begin
    if (!p.font eq 1) then ytt='!9l!4!u2!nF!9!dn!4!n (Jy !9m!4m!u2!n)' $
      else ytt='!7k!5!u2!nF!7!dm!5!n (Jy !7l!5m!u2!n)'
  endif else if (flamflag eq 1) then begin
    if (!p.font eq 1) then ytt='!4F!9!dl!4!n (W m!u-2!n !9m!4m!u-1!n)' $
      else ytt='!5F!7!dk!5!n (W m!u-2!n !7l!5m!u-1!n)' 
  endif else if (lamflag eq 1) then begin
    if (!p.font eq 1) then ytt='!9l!4F!9!dl!4!n (W m!u-2!n)' $
      else ytt='!7k5F!7!dk!5!n (W m!u-2!n)'
  endif else begin
    if (!p.font eq 1) then ytt='!4F!9!dn!4!n (Jy)' $
      else ytt='!5F!7!dm!5!n (Jy)'
  endelse
endif else ytt=ytitle

;xtt='!9l!4 (!9m!4m)'
;ytt='!9l!4!u2!nF!9!dn!4!n (Jy !9m!4m!u2!n)'
;ytt='!4F!9!dn!4!n (mJy)'

; check size of array, load wavelength and flux
; set yrange if not set already - data_array now limited to data to be plotted
; if rj or flam keywords set, modify flux

ncol=n_elements(data_array[*,0])
nlen=n_elements(data_array[0,*])
l=reform(test_array[lcol,*])
f=reform(test_array[fcol,*])

if (rjflag eq 1) then begin
  f=f*l*l
endif else if (flamflag eq 1) then begin
  f=f*3e-12/(l*l)
endif else if (lamflag eq 1) then begin
  f=f*3e-12/l
endif

fin_idx=where(finite(f) eq 1)
if (keyword_set(yrange) eq 0 and n_elements(fin_idx) gt 1) then $
  yrange=[min(f[fin_idx]),max(f[fin_idx])]

; check to see if spectrum has segments
; if only two columns then set minseg=maxseg=0

if (ncol gt ocol) then begin
  minseg=min(data_array[ocol,*])
  maxseg=max(data_array[ocol,*])
endif else begin
  minseg=0
  maxseg=0
endelse

; plot it up

if (minseg lt maxseg)  then begin ; we have segmented spectral data

  segcount=0
  for n=minseg,maxseg do begin
    segidx=where(data_array[ocol,*] eq n)
    if (max(segidx) gt -1) then begin
      l=reform(data_array[lcol,segidx])
      f=reform(data_array[fcol,segidx])
      if (rjflag eq 1) then begin
        f=f*l*l
      endif else if (flamflag eq 1) then begin
        f=f*3e-12/(l*l)
      endif
      if (segcount eq 0 and overflag eq 0) then begin
        if (dataflag eq 1) then begin
          plot,l,f,xtit=xtt,ytit=ytt,xran=xrange,yran=yrange,_extra=e
	endif else begin
          plot,l,f,xtit=xtt,ytit=ytt,xran=xrange,yran=yrange,/nodata,_extra=e
	endelse
      endif else if (dataflag eq 1) then oplot,l,f,_extra=e
      segcount=segcount+1
    endif
  endfor

endif else begin                 ; spectrum is unsegmented

  if (overflag eq 0) then begin
    if (dataflag eq 1) then $
      plot,l,f,xtit=xtt,ytit=ytt,xran=xrange,yran=yrange,_extra=e $
      else $
      plot,l,f,xtit=xtt,ytit=ytt,xran=xrange,yran=yrange,/nodata,_extra=e
  endif else oplot,l,f,_extra=e

endelse

END
