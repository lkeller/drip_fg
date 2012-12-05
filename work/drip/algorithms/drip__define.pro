; NAME:
;     DRIP - Version .8.0
;
; PURPOSE:
;     Data Reduction Interactive Pipeline
;
; CALLING SEQUENCE:
;     Obj=Obj_new('DRIP', FILELIST)
;     Structure=Obj->RUN(FILELIST)
;
; INPUTS:
;     FILELIST - Filename(s) of the fits file(s) containing data to be reduced.
;                May be a string array
;
; STRUCTURE:
;     {mode, n, readme, set                                              - settings
;      path, filename, header, basehead                                  - file info
;      data, cleaned, badflags, linearized, flatted, merged, undistorted - pipe steps
;      coadded, lastcoadd                                                - final products
;      badmap, linear_coeffs, flats, cleanedflats, masterflat}           - calibration
;
; OUTPUTS:
;     IDL output: full data structure(s).
;
;     FITS output: final coadded result or individual step results
;
; CALLED ROUTINES AND OBJECTS:
;     SXPAR
;     SXADDPAR
;     FITS_READ
;     READFITS
;     FITS_WRITE
;
; SIDE EFFECTS:
;     None identified
;
; RESTRICTIONS:
;     Data (FITS) file or dripconfig.txt file must specify locations
;     of bad pixel map, dark frames, and flat fields.
;
; PROCEDURE:
;     Upon initialization get mode. For each file check if the mode is
;     correct and reduce the file. All files are coadded. Run returns
;     the data structure.
;     Save saves _reduced.fits files with additional data reduction keywords
;
; MODIFICATION HISTORY:
;     Written by:  Alfred Lee, Cornell University, June, 2002
;     Modified:   Alfred Lee, CU, June 18, 2002
;                 added two keywords for TPCON and TPCOFF.  Made both routines
;                 functional
;     Modified:   Alfred Lee, CU, June 21, 2002
;                 adjusted code for new TPC class.
;     Modified:   Alfred Lee, CU, June 24, 2002
;                 adjusted code for changes in TPC.  Allowed for multiple file
;                 input.
;     Modified:   Alfred Lee, CU, June 25, 2002
;                 DRIP now saves a fits file containing the averaged
;                 sum of the final images from every inputted file.
;                 added PRINT statements.
;     Modified:   Alfred Lee, CU, June 27, 2002
;                 Added a NO_SAVE keyword to skip saving, which takes
;                 much time.
;     Modified:   Alfred Lee, CU, July 11, 2002
;                 Converted into an Object to be used with DRIP_GUI.
;     Modified:   Alfred Lee, CU, September 27, 2002
;                 Refined for use with GUI.  Uses fewer assumed parameters,
;                 and checks for more.
;     Modified:   Alfred Lee, CU, October 8, 2002
;                 Fixed GETDATA to be more functional.
;     Modified:   Alfred Lee, CU, January 30, 2003
;                 updated save routine
;     Rewritten:  Marc Berthoud, CU, June 2004
;                 Merged former drip(4.2), tpc(5.1) and dataman(3)
;                 objects to get single drip object
;     Modified:   Marc Berthoud, CU, August 2004
;                 Have run be a procedure and getdata (no params)
;                 return all the data (no pointers)
;     Modified:   Marc Berthoud, CU, May 2005
;                 Added use of pathload, pathsave instead of path
;                 Added use of badfile and flatfile
;                 * if badfile and flatfile are changed after first
;                   run() then getcal() has to be called to load them
;     Modified:   Marc Berthoud, CU, August 2007
;                 Added catch if flatfile or badmask doesn't open
;                 Added use of darkfile: flat=flatfile-darkfile
;                 Added loading of drip configuration file
;    
;     Modified:   Luke Keller, IC, August 2009
;                 Changed flatfield prep to use standardized flat file format
;
;     Modified:   Luke Keller, IC, January 2010
;                 Added final non-linearity correction
;
;     Modified    Luke Keller, IC, March 2010
;                 Added use of MASKFILE to mask flatfield files 
;                 for use in grism mode (getcal)
;                 Added 'darksum' step to drip structure
;     Modified    Luke Keller, IC, May 2 2010
;                 Added drip::findcal to search for calibration data in the
;                 current data directory
;     Modified    Miguel Charcos Llorens, USRA, April 2011
;                 Modified definition of DRIP in order to host distortion correction
;                 Update setdata and getdata accordingly
;                 Include correct call to the new drip_undistort version
;                 Include distortion calculations in getcal
;                 Include droop step in drip and update setdata and getdata
;                 Added work directory where the code is running in order to consider cal files in ../cal/ wrt this directory
;     Modified    Miguel Charcos Llorens, USRA, May 2011
;                 Added global linearity correction step
;                 Include check of LWC or SWC camera
;     Modified    Luke Keller, Ithaca College, June 2011
;                 Added C2NC2 observing mode
;     Modified:   Luke Keller. Ithaca College, November 2011
;                 Intermediate pipeline data products now have correct
;                 headers containing processing history

;****************************************************************************
;     SETDATA - set data values
;****************************************************************************

pro drip::setdata, mode=mo, n=n, coaddN=coaddN, lastlistN= lastlistN, readme=rm, $
        pathcal=wo, pathload=pl, pathsave=ps, pathcalsave=pc, filename=fn, header=hd, lastheader=lhd, basehead=bh, $
        badfile=bf, linfile=lnf, flatfile=ff, data=da, ditherp=dp, $  ;LIN
        cleaned=cl, drooped=dr, imglinearized=il, badflags=bl, linearized=lnr, flatted=fl, stacked=st, undistorted=ud, $  ;LIN
        merged=md, coadded=coa, coadded_rot=cor, lastcoadd=lc, lastlistcoadd=llc, mergestack=ms, $ ; MERGESTACK
	lastmergestack=lms, lastditherp=ldp, lastlistmergestack=llms, lastlistditherp=lldp, $
        lastcoaddN=lcn, lastlistcoaddN=llcn,  $
	badmap=bm, darks=ds, cleanddarks=cd, darksum=dks, pinpos=pinp, $
	flats=fs, cleanedflats=cf, droopedflats=df, imglinearizedflats=imf, $
        masterflat=mf, lincor=lnc ;LIN

if keyword_set(mo) then self.mode=mo
if keyword_set(n) then  self.n=n
if keyword_set(lastlistN) then  self.lastlistN=lastlistN
if keyword_set(coaddN) then  self.coaddN=coaddN
if keyword_set(rm) then self.readme=rm
if keyword_set(wo) then self.pathcal=wo
if keyword_set(pl) then self.pathload=pl
if keyword_set(ps) then self.pathsave=ps
if keyword_set(pc) then self.pathcalsave=pc
if keyword_set(fn) then self.filename=fn
if keyword_set(hd) then *self.lastheader=lhd
if keyword_set(lhd) then *self.header=hd
if keyword_set(bh) then *self.basehead=bh
if keyword_set(bf) then *self.badfile=bf
if keyword_set(lnf) then *self.linfile=lnf  ;LIN
if keyword_set(ff) then *self.flatfile=ff
if keyword_set(da) then *self.data=da
if keyword_set(cl) then *self.cleaned=cl
if keyword_set(dr) then *self.drooped=dr
if keyword_set(il) then *self.imglinearized=il
if keyword_set(bl) then *self.badflags=bl
if keyword_set(lnr) then *self.linearized=lnr  ;LIN
if keyword_set(fl) then *self.flatted=fl
if keyword_set(st) then *self.stacked=st
if keyword_set(ud) then *self.undistorted=ud
if keyword_set(md) then *self.merged=md
if keyword_set(ms) then *self.mergestack=ms  ; MERGESTACK
if keyword_set(dp) then *self.ditherp=dp
if keyword_set(coa) then *self.coadded=coa
if keyword_set(cor) then *self.coadded_rot=cor
if keyword_set(lc) then *self.lastcoadd=lc
if keyword_set(llc) then *self.lastlistcoadd=llc
if keyword_set(lms) then *self.lastmergestack=lms 
if keyword_set(ldp) then *self.lastditherp=ldp
if keyword_set(llms) then *self.lastlistmergestack=llms 
if keyword_set(lldp) then *self.lastlistditherp=lldp
if keyword_set(lcn) then  self.lastcoaddN=lcn
if keyword_set(llcn) then  self.lastlistcoaddN=llcn
if keyword_set(bm) then *self.badmap=bm
if keyword_set(fs) then *self.darks=ds
if keyword_set(cd) then *self.cleaneddarks=cd
if keyword_set(dks) then *self.darksum=dks
if keyword_set(pinp) then *self.pinpos=pinp
if keyword_set(fs) then *self.flats=fs
if keyword_set(lnc) then *self.lincor=lnc     ;LIN
if keyword_set(cf) then *self.cleanedflats=cf
if keyword_set(df) then *self.droopedflats=df
if keyword_set(imf) then *self.imglinearizedflats=imf
if keyword_set(mf) then *self.masterflat=mf

end

;****************************************************************************
;     GETDATA - Returns SELF structure as a non-object or specified variables
;****************************************************************************

function drip::getdata, mode=mo, n=n, lastlistN=lastlistN, coaddN=coaddN, readme=rm, $
             pathcal=wo, pathload=pl, pathsave=ps, pathcalsave=pc, filename=fn, header=hd, lastheader=lhd, basehead=bh, $
             badfile=bf, linfile=lnf, flatfile=ff, data=da, drooped=dr, imglinearized=il, $  ;LIN
             cleaned=cl, badflags=bl, linearized=lnr, flatted=fl, stacked=st, undistorted=ud, $ ;LIN
             merged=md, coadded=coa, coadded_rot=cor, lastcoadd=lc, lastlistcoadd=llc, mergestack=ms, $ ; MERGESTACK
	     lastmergestack=lms, lastditherp=ldp, lastlistmergestack=llms, lastlistditherp=lldp, $
             lastcoaddN=lcn, lastlistcoaddN=llcn,  $
	     ditherp=dp, badmap=bm, darks=ds, cleaneddarks=cd, darksum=dks, pinpos=pinp, $
	     flats=fs, cleanedflats=cf, droopedflats=df,imglinearizedflats=imf,$
             masterflat=mf, lincor=lnc ;LIN

if keyword_set(mo) then return, self.mode
if keyword_set(n)  then return, self.n
if keyword_set(coaddN)     then return, self.coaddN
if keyword_set(lastlistN)  then return, self.lastlistN
if keyword_set(rm) then return, self.readme
if keyword_set(wo) then return, self.pathcal
if keyword_set(pl) then return, self.pathload
if keyword_set(ps) then return, self.pathsave
if keyword_set(pc) then return, self.pathcalsave
if keyword_set(fn) then return, self.filename
if keyword_set(hd) then return, *self.header
if keyword_set(lhd) then return, *self.lastheader
if keyword_set(bh) then return, *self.basehead
if keyword_set(bf) then return, *self.badfile
if keyword_set(lnf) then return, *self.linfile  ;LIN
if keyword_set(ff) then return, *self.flatfile
if keyword_set(da) then return, *self.data
if keyword_set(cl) then return, *self.cleaned
if keyword_set(dr) then return, *self.drooped
if keyword_set(il) then return, *self.imglinearized
if keyword_set(bl) then return, *self.badflags
if keyword_set(lnr) then return, *self.linearized  ;LIN
if keyword_set(fl) then return, *self.flatted
if keyword_set(st) then return, *self.stacked
if keyword_set(ud) then return, *self.undistorted
if keyword_set(md) then return, *self.merged
if keyword_set(ms) then return, *self.mergestack ; MERGESTACK
if keyword_set(dp) then return, *self.ditherp
if keyword_set(coa) then return, *self.coadded
if keyword_set(cor) then return, *self.coadded_rot
if keyword_set(lc) then return, *self.lastcoadd
if keyword_set(llc) then return, *self.lastlistcoadd
if keyword_set(lms) then return, *self.lastmergestack 
if keyword_set(ldp) then return, *self.lastditherp
if keyword_set(llms) then return, *self.lastlistmergestack 
if keyword_set(lldp) then return, *self.lastlistditherp
if keyword_set(lcn)  then return, self.lastcoaddN
if keyword_set(llcn)  then return, self.lastlsitcoaddN
if keyword_set(bm) then return, *self.badmap
if keyword_set(ds) then return, *self.darks
if keyword_set(cd) then return, *self.cleaneddarks
if keyword_set(dks) then return, *self.darksum
if keyword_set(pinp) then return, *self.pinpos
if keyword_set(fs) then return, *self.flats
if keyword_set(lnc) then return, *self.lincor   ;LIN
if keyword_set(cf) then return, *self.cleanedflats
if keyword_set(df) then return, *self.droopedflats
if keyword_set(imf) then return, *self.imglinearizedflats
if keyword_set(mf) then return, *self.masterflat

structure={mode:self.mode, n:self.n, coaddN:self.coaddN, lastlistN:self.lastlistN, readme:self.readme, $ 
           header:*self.header, lastheader:*self.lastheader, basehead:*self.basehead, filename:self.filename, $
           pathcal:self.pathcal,pathload:self.pathload, pathsave:self.pathsave, pathcalsave:self.pathcalsave, $
           badfile:self.badfile, linfile:self.linfile, flatfile:self.flatfile, $ ;LIN
           data:*self.data, drooped:*self.drooped, imglinearized:*self.imglinearized, cleaned:*self.cleaned, badflags:*self.badflags, $
           linearized:*self.linearized, flatted:*self.flatted, stacked:*self.stacked, $ ;LIN
           undistorted:*self.undistorted, merged:*self.merged, mergestack:*self.mergestack, $  ; MERGESTACK
           ditherp:*self.ditherp, coadded:*self.coadded, coadded_rot:rot(*self.coadded,90), $
           lastcoadd:*self.lastcoadd, lastlistcoadd:*self.lastlistcoadd, badmap:*self.badmap, darks:*self.darks, $
           lastmergestack:*self.lastmergestack, lastditherp:*self.lastditherp, $
           lastlistmergestack:*self.lastlistmergestack, lastlistditherp:*self.lastlistditherp, $
	   lastcoaddN:self.lastcoaddN, lastlistcoaddN:self.lastlistcoaddN, $
	   cleaneddarks:*self.cleaneddarks, darksum:*self.darksum, pinpos:*self.pinpos, flats:*self.flats, $
           lincor:*self.lincor, $ ;LIN
           cleanedflats:*self.cleanedflats, droopedflats:*self.droopedflats, imglinearizedflats:*self.imglinearizedflats, masterflat:*self.masterflat, $
           posdata:*self.posdata, chopsub:*self.chopsub, $
           nodsub:*self.nodsub}
return, structure

end

;******************************************************************************
;     LOAD - load intermediate steps from FITS image.
;******************************************************************************

pro drip::load, filename, masterflat=mf, drooped=dr, imglinearized=il, cleaned=cl, linearized=lnr, flatted=fl, stacked=st, $ ;LIN
                undistort=ud, merged=md, coadded=coa, coadded_rot=cor

; error check
s=size(filename)
if (s[0] ne 0) and (s[1] ne 7) then begin
    drip_message, 'drip::load - Must provide filename'
    return
endif
; make output filename and set data
self.filename=filename
namepos=strpos(self.filename,'.fit',/reverse_search)
if keyword_set(mf) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_masterflat.fits'
    data=self.masterflat
    head=self.header
endif else if keyword_set(cl) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_cleaned.fits'
    data=self.cleaned
    head=self.header
endif else if keyword_set(dr) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_drooped.fits'
    data=self.drooped
    head=self.header
endif else if keyword_set(il) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_imglinearized.fits'
    data=self.imglinearized
    head=self.header
endif else if keyword_set(lnr) then begin   ;LIN
    fname=self.pathload+strmid(self.filename,0,namepos)+'_linearized.fits'
    data=self.linearized
    head=self.header    
endif else if keyword_set(fl) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_flatted.fits'
    data=self.flatted
    head=self.header
endif else if keyword_set(st) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_stacked.fits'
    data=self.stacked
    head=self.header
endif else if keyword_set(ud) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_undistorted.fits'
    data=self.undistorted
    head=self.header
endif else if keyword_set(md) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_merged.fits'
    data=self.merged
    head=self.header
endif else if keyword_set(coa) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_coadded.fits'
    data=self.coadded
    head=self.basehead
endif else if keyword_set(cor) then begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_coadded_rot.fits'
    data=self.coadded_rot
    head=self.basehead
endif else begin
    fname=self.pathload+strmid(self.filename,0,namepos)+'_reduced.fits'
    data=self.coadded
    head=self.basehead
endelse
; read file
*data=readfits(fname,*head,/noscale,/silent)
end

;******************************************************************************
;     SAVE - save final output or intermediate steps as FITS image.
;******************************************************************************

pro drip::save, all=all, masterflat=mf, cleaned=cl, drooped=dr, imglinearized=il, linearized=lnr, flatted=fl, stacked=st, $ ;LIN
                undistort=ud, merged=md, coadded=co, coadded_rot= cor, $
                filename=filename

; check if reduced data available
if self.n lt 1 then begin
    drip_message, 'drip::save - no reduced data to save'
    return
endif
; make output filename and set data
namepos=strpos(self.filename,'.fit',/reverse_search)
if keyword_set(all) then begin
    ;resize=2.0 ;2
    ;border=128 ;128
    resizeread = drip_getpar(basehead,'RESIZE')
    if resizeread eq 'x' then resize=2.0 $
    else resize = float(resizeread)
    borderread = drip_getpar(basehead,'BORDER')
    if borderread eq 'x' then border=128 $
    else border = fix(borderread)

    fname=strmid(self.filename,0,namepos)+'_all.fits'
    nx = 0
    ny = 0
    data=ptr_new(/allocate_heap)
    nframes = 0
    s=size(*self.masterflat)
    sframes = 0
    if s[0] eq 2 then begin
      sframes = 1
      nx = s[1]
      ny = s[2]
      *data = fltarr(nx,ny,sframes)
      (*data)[*,*,nframes:nframes+sframes-1] = *self.masterflat
      nframes = nframes + sframes
    endif    
    
    s=size(*self.darksum)
    sframes = 0
    if s[0] eq 2 then begin
      sframes = 1
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	(*data)[*,*,nframes:nframes+sframes-1] = *self.darksum
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	datacube[*,*,nframes:nframes+sframes-1] = *self.darksum
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif    
    
    s=size(*self.cleaned)
    sframes = 0
    if s[0] eq 2 then sframes = 1 $
    else if s[0] eq 3 then sframes = s[3]
    if sframes ne 0 then begin
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	(*data)[*,*,nframes:nframes+sframes-1] = *self.cleaned
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	datacube[*,*,nframes:nframes+sframes-1] = *self.cleaned
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif
        
    s=size(*self.drooped)
    sframes = 0
    if s[0] eq 2 then sframes = 1 $
    else if s[0] eq 3 then sframes = s[3]
    if sframes ne 0 then begin
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	(*data)[*,*,nframes:nframes+sframes-1] = *self.drooped
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	datacube[*,*,nframes:nframes+sframes-1] = *self.drooped
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif
        
    s=size(*self.imglinearized)
    sframes = 0
    if s[0] eq 2 then sframes = 1 $
    else if s[0] eq 3 then sframes = s[3]
    if sframes ne 0 then begin
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	(*data)[*,*,nframes:nframes+sframes-1] = *self.imglinearized
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	datacube[*,*,nframes:nframes+sframes-1] = *self.imglinearized
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif
    
    s=size(*self.flatted)
    sframes = 0
    if s[0] eq 2 then sframes = 1 $
    else if s[0] eq 3 then sframes = s[3]
    if sframes ne 0 then begin
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	(*data)[*,*,nframes:nframes+sframes-1] = *self.flatted
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	datacube[*,*,nframes:nframes+sframes-1] = *self.flatted
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif
    
    s=size(*self.linearized)
    sframes = 0
    if s[0] eq 2 then sframes = 1 $
    else if s[0] eq 3 then sframes = s[3]
    if sframes ne 0 then begin
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	(*data)[*,*,nframes:nframes+sframes-1] = *self.linearized
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	datacube[*,*,nframes:nframes+sframes-1] = *self.linearized
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif
    
    s=size(*self.stacked)
    sframes = 0
    if s[0] eq 2 then sframes = 1 $
    else if s[0] eq 3 then sframes = s[3]
    if sframes ne 0 then begin
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	(*data)[*,*,nframes:nframes+sframes-1] = *self.stacked
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	datacube[*,*,nframes:nframes+sframes-1] = *self.stacked
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif
    
    s=size(*self.undistorted)
    sframes = 0
    if s[0] eq 2 then sframes = 1 $
    else if s[0] eq 3 then sframes = s[3]
    if sframes ne 0 then begin
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	;(*data)[*,*,nframes:nframes+sframes-1] = (*self.undistorted)[border:border+nx-1,border:border+ny-1]
	(*data)[*,*,nframes:nframes+sframes-1] = (*self.undistorted)[border+fix((resize-1)*nx)/2:border+fix((resize-1)*nx)/2+nx-1,  $
        							     border+fix((resize-1)*ny)/2:border+fix((resize-1)*ny)/2+ny-1]
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	;datacube[*,*,nframes:nframes+sframes-1] = (*self.undistorted)[border:border+nx-1,border:border+ny-1]
	datacube[*,*,nframes:nframes+sframes-1] = (*self.undistorted)[border+fix((resize-1)*nx)/2:border+fix((resize-1)*nx)/2+nx-1,  $
        							      border+fix((resize-1)*ny)/2:border+fix((resize-1)*ny)/2+ny-1]
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif
    
    s=size(*self.merged)
    sframes = 0
    if s[0] eq 2 then sframes = 1 $
    else if s[0] eq 3 then sframes = s[3]
    if sframes ne 0 then begin
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	;(*data)[*,*,nframes:nframes+sframes-1] = (*self.merged)[border:border+nx-1,border:border+ny-1]
	(*data)[*,*,nframes:nframes+sframes-1] = (*self.merged)[border+fix((resize-1)*nx)/2:border+fix((resize-1)*nx)/2+nx-1,  $
        							border+fix((resize-1)*ny)/2:border+fix((resize-1)*ny)/2+ny-1]
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	;datacube[*,*,nframes:nframes+sframes-1] = (*self.merged)[border:border+nx-1,border:border+ny-1]
	datacube[*,*,nframes:nframes+sframes-1] = (*self.merged)[border+fix((resize-1)*nx)/2:border+fix((resize-1)*nx)/2+nx-1,  $
        							 border+fix((resize-1)*ny)/2:border+fix((resize-1)*ny)/2+ny-1]
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif
    
    s=size(*self.coadded)
    sframes = 0
    if s[0] eq 2 then sframes = 1 $
    else if s[0] eq 3 then sframes = s[3]
    if sframes ne 0 then begin
      if nx eq 0 then begin
	nx = s[1]
	ny = s[2]
	(*data)[*,*,nframes:nframes+sframes-1] = (*self.coadded)
      endif else begin
        datacube = fltarr(nx,ny,nframes+sframes)
	datacube[*,*,0:nframes-1] = *data
	datacube[*,*,nframes:nframes+sframes-1] = (*self.coadded)
	*data = datacube
      endelse
      nframes = nframes + sframes
    endif
    
endif else if keyword_set(mf) then begin
    fname=strmid(self.filename,0,namepos)+'_masterflat.fits'
    data=self.masterflat
endif else if keyword_set(cl) then begin
    fname=strmid(self.filename,0,namepos)+'_cleaned.fits'
    data=self.cleaned
endif else if keyword_set(dr) then begin
    fname=strmid(self.filename,0,namepos)+'_drooped.fits'
    data=self.drooped
endif else if keyword_set(il) then begin
    fname=strmid(self.filename,0,namepos)+'_imglinearized.fits'
    data=self.imglinearized
endif else if keyword_set(lnr) then begin ; LIN
    fname=strmid(self.filename,0,namepos)+'_linearized.fits'
    data=self.linearized    
endif else if keyword_set(fl) then begin
    fname=strmid(self.filename,0,namepos)+'_flatted.fits'
    data=self.flatted
endif else if keyword_set(st) then begin
    fname=strmid(self.filename,0,namepos)+'_stacked.fits'
    data=self.stacked
endif else if keyword_set(ud) then begin
    fname=strmid(self.filename,0,namepos)+'_undistorted.fits'
    data=self.undistorted
endif else if keyword_set(md) then begin
    fname=strmid(self.filename,0,namepos)+'_merged.fits'
    data=self.merged
endif else if keyword_set(co) then begin
    fname=strmid(self.filename,0,namepos)+'_coadded.fits'
    data=self.coadded
endif else if keyword_set(cor) then begin
    fname=strmid(self.filename,0,namepos)+'_coadded_rot.fits'
    data=self.coadded_rot
endif else begin
    fname=strmid(self.filename,0,namepos)+'_reduced.fits'
    data=self.coadded
 endelse
; add pathsave to fname if there is no full path in fname
if strpos(self.filename,path_sep()) lt 0 then fname = self.pathsave + fname
; check for overriding filename selection
if keyword_set(filename) then fname=filename
; save file
;fits_write,fname,*data,*self.basehead
;fits_write,fname,*data,*self.lastheader
fits_write,fname,*data,*self.basehead


                        ; Extract processing HISTORY out of
                                ; self.basehead and add it to
                                ; self.header for saving intermediate
                                ; data products
        ;help,*self.basehead
        ;base_header = *self.basehead
        ;proc_hist=base_header(where(strmatch(*self.basehead, 'HISTORY*') eq 1))  ; Extract HISTORY keywords and data
        ;header_header = *self.header
        
        ;header_header = header_header[0:n_elements(header_header)-2]  ; strip END keyword off
        ;int_write_header = [header_header, proc_hist, 'END     ']          ; add HISTORY to the end of self.header and add END
        ;print,int_write_header
        
        ;if strmatch(fname,'*coadd*') eq 1 then int_write_header = *self.basehead  ; use base header for final coadded image(s)
	;fits_write,fname,*data, int_write_header




drip_message,'saved reduced data file: '+fname
; free memory space for data pointer
if ptr_valid(data) then ptr_free,data
end


;******************************************************************************
;     CREATEPRODUCTS - returns data cube with calibration or final products pointers
;                      correspontind to the input names
;                      WARNING: This creates a new heap that has to be 
;                      remove by the task calling createproducts to free memory
;******************************************************************************
FUNCTION drip::createproducts, innames, cal=cal
  
  if keyword_set(cal) then refdata = self.callist $
  else refdata = self.reducedlist
  
  names =strlowcase(innames)
  Nnames = n_elements(names)
  found = replicate(0,Nnames)
  rescube = replicate({name:'',pointer:ptr_new()},Nnames)
  
  for i=0, n_elements(*refdata)-1 do begin
    k = where(names eq strlowcase(((*refdata)(i)).name))
    if k(0) ne -1 then begin
      found(k(0)) = 1
      ; Here we copy the information of the structure to the new array
      ; Which means that
      ;    - It takes more space.
      ;    - If we remove the rescube=returnedcube we do not remove refdata
      rescube(k(0)) = (*refdata)(i)
    endif
  endfor
  
  k = where(found eq 1)
  
  returnedcube = ptr_new(/allocate_heap)
  *returnedcube = {name:'',pointer:ptr_new()}
  if k(0) eq -1 then return, returnedcube
  
  *returnedcube = rescube(k)
  
  return, returnedcube
  
END

;******************************************************************************
;     SAVEPRODUCTS - save final output or intermediate steps as FITS image.
;               the difference with save is that this assumes a directory
;               structure for produced calibration and reduced data
;               It also uses a new approach which consist on using the list 
;               named self.reducedlist. The calibration products are saved at
;               self.pathcalsave and the reduced products are saved at self.pathsave
;
;               USAGE:
;                 - names: string array with the name of the products to be saved
;                          There are three special inputs: ALL, RED and CAL
;                          ALL - save all calibration and reduced images
;                          RED - save all the reduced images
;                          CAL - save all the calibration images
;                 - output: output type. It should be CUBE|CALCUBE|REDCUBE|BOTHCUBE
;                           if output is missed or different to these values, all the 
;                           outputs are saved as individual files in their appropriate dir
;                           CUBE - creates a unique data cube with all the products
;                                  that is saved at the reduced directory if
;                                  any reduced data or calibration directory if none
;                           CALCUBE - creates a cube only with the calibration data
;                                     that is saved at the cal directory
;                                     The reduced data are saved as individual files
;                           REDCUBE - creates a cube only with the reduced data
;                                     that is saved at the reduced directory
;                                     The calibration data are saved as individual files
;                           BOTHCUBE - creates two cubes for each reduced and calibration data
;                                     that are saved in their corresponding directory
;                 - filename: Output filename. If no cube selected this is used
;                             as root to create filenames for each product
;               
;******************************************************************************

PRO drip::saveproducts, innames, output=output, filename=filename, cut256x256=cut256x256

  ; check if reduced data available
  if self.n lt 1 then begin
      drip_message, 'drip::save - no reduced data to save'
      return
  endif
  
  ; Define properties when big images
  resizeread = drip_getpar(*self.basehead,'RESIZE')
  if resizeread eq 'x' then resize=2.0 $
  else resize = float(resizeread)
  borderread = drip_getpar(*self.basehead,'BORDER')
  if borderread eq 'x' then border=128 $
  else border = fix(borderread)
  
  ; initialize image size (this could be more general 
  ; if we read the data somewhere and we do not assume 256x256
  nx = 256
  ny = 256
  
  if keyword_set(cut256x256) eq 0 then begin
    newnx = 2*border+fix((resize)*nx)
    newny = 2*border+fix((resize)*ny)
  endif
    
  ; make output filename root
  if keyword_set(filename) then rootname=filename $
  else rootname=self.filename
  namepos=strpos(self.filename,'.fit',/reverse_search)
  rootname = strmid(rootname,0,namepos)
  namepos=strpos(rootname,path_sep(),/reverse_search)
  if namepos ge 0  then rootname = strmid(rootname,namepos+1,strlen(rootname))
  drip_message, 'drip::save - file name root is '+rootname
  
  names = strlowcase(innames)
  
  ; find data to be saved
  ; If a new structure array is created we will have to remove the heap 
  ; But if the pointer is copied from self.reducedlist and self.callist
  ; this should not be removed since it would remove the latests as well
  ; In summary: if names=all redprods and calprods does not have to be removed
  ; *** BUT ***
  ; Ok, because the c2nc2 mode requires to remove the merge step, I will create
  ; a new heap for reprods and calprods even in the case where all is choose
  ; This will avoid making a test case at the end when freeing the heap
  k = where(names eq 'all')
  if k(0) ne -1 then begin
    redprods = ptr_new(/allocate_heap)
    calprods = ptr_new(/allocate_heap)
    *redprods = *(self.reducedlist)
    *calprods = *(self.callist)   
  endif else begin
    kred= where(names eq 'red')
    if kred(0) eq -1 then redprods = self->createproducts(names) $
    else redprods = self.reducedlist
    kcal= where(names eq 'cal')
    if kcal(0) eq -1 then calprods = self->createproducts(names,/cal) $
    else calprods = self.callist    
  endelse
  
  ; If c2nc2 case then remove merge step since in practice it does not exist
  ; We assume that merge can be contained only once. It is probably easy to 
  ; create a more general case but it is very unlikely that the same product appears
  ; more than once
  if self.mode eq 'C2NC2' then begin    
    if ((*redprods)[0]).name eq 'MERGED' then begin
      if n_elements(*redprods) gt 1 then *redprods = (*redprods)[1:n_elements(*redprods)-1] $
      else *redprods = {name:'',pointer:ptr_new()}
    endif else begin
      if (((*redprods)[n_elements(*redprods)-1]).name) eq 'MERGED' then begin
        ; here the number of elements should be greater than 1 because 
	; if not the index would be 0 and we would be in the previous case
	*redprods = (*redprods)[0:n_elements(*redprods)-2]
      endif else begin
	foundmerge = 0
	for i=1, n_elements(*redprods)-1 do begin
	  if ((*redprods)(i)).name eq 'MERGED' then begin
            tmpredprods = [(*redprods)[0:i-1],(*redprods)[i+1:n_elements(*redprods)-1]]
	    foundmerge = 1
	  endif
	endfor
	if foundmerge eq 1 then *redprods=tmpredprods
      endelse
    endelse
  endif
  
  if ((*redprods)(0).pointer eq ptr_new() && (*calprods)(0).pointer eq ptr_new()) then begin
      drip_message, 'drip::save - this data is not available to be saved'
      ptr_free,redprods
      ptr_free,calprods
      return
  endif
  
  ; initialize output=none if keyword does not exist
  if (keyword_set(output) eq 0) then output='NONE'
  output = strupcase(output)
  
  ; Change the output if the saved data will be only one
  CASE output of
    'CALCUBE': begin
		 if n_elements(*calprods) eq 1 then output = 'NONE'
		 if n_elements(*calprods) eq 0 then begin
		   drip_message, 'drip::save - no data available for output mode '+strupcase(output)
		   return
		 endif
              end
    'REDCUBE': begin
		 if n_elements(*redprods) eq 1 then output = 'NONE'
		 if n_elements(*redprods) eq 0 then begin
		   drip_message, 'drip::save - no data available for output mode '+strupcase(output)
		   return
		 endif
              end
    'CUBE': begin
	      if n_elements(*calprods)+n_elements(*redprods) eq 1 then output = 'NONE'
	      if n_elements(*calprods)+n_elements(*redprods) eq 0 then begin
		drip_message, 'drip::save - no data available for output mode '+strupcase(output)
		return
	      endif
           end
    'BOTHCUBE': begin
		  if n_elements(*calprods) eq 1 && n_elements(*redprods) gt 1 then output = 'REDCUBE'
		  if n_elements(*calprods) gt 1 && n_elements(*redprods) eq 1 then output = 'CALCUBE'
		  if n_elements(*calprods) eq 1 && n_elements(*redprods) eq 1 then output = 'NONE'
		  if n_elements(*calprods)+n_elements(*redprods) eq 0 then begin
		    drip_message, 'drip::save - no data available for output mode '+strupcase(output)
		    return
		  endif
               end
    else:
  ENDCASE
   
  totalframes = 0
  
  ;save data for calibration products
  if ((*calprods)(0).pointer ne ptr_new()) then begin
    if (output eq 'CUBE' || output eq 'BOTHCUBE' || output eq 'CALCUBE') then begin
      cubecal = ptr_new(/allocate_heap)
      nframes = 0
    endif
    
    if (output ne 'CUBE') then begin
      if strpos(rootname,path_sep()) lt 0 then begin
        drip_message,'Directory for calibration data file: '
        drip_message,'      '+self.pathcalsave
      endif else begin
        drip_message,'Directory for calibration data file: '
        drip_message,'      '+strmid(rootname,0,strpos(rootname,path_sep()))
      endelse
    endif 
    
    calframes = intarr(n_elements(*calprods))
    for i=0,n_elements(*calprods)-1 do begin
      data = (*calprods)(i).pointer
      if (output eq 'CUBE' || output eq 'BOTHCUBE' || output eq 'CALCUBE') then begin
	s=size(*data)
	sframes = 0
	if s[0] eq 2 then sframes = 1 $
	else if s[0] eq 3 then sframes = s[3]
	
	; complete calframes which will keep track of the number of frames 
	; for each type of data. This will be used to write what data 
	; is in each frame
	if i gt 0 then calframes(i)=calframes(i-1)+sframes $
	else calframes(0) = sframes-1
	
	if sframes ne 0 then begin
	  if (s[1] ge nx && s[2] ge ny) then begin 
	    if nframes eq 0 then begin
	      if keyword_set(cut256x256) then begin
		if (s[1] gt nx || s[2] gt ny) then *cubecal = *data[border+fix((resize-1)*nx)/2:border+fix((resize-1)*nx)/2+nx-1,  $
                                                                    border+fix((resize-1)*ny)/2:border+fix((resize-1)*ny)/2+ny-1] $
		else *cubecal = *data
	      endif else begin
		if (s[1] lt newnx || s[2] lt newny) then begin
		  newdata = replicate(0.,newnx,newny,sframes)
		  newdata[newnx/2-s[1]/2:newnx/2+s[1]/2-1,newny/2-s[2]/2:newny/2+s[2]/2-1,*] = *data
		  *cubecal = newdata
		endif else begin
		  *cubecal = *data
	    	endelse
	      endelse
	    endif else begin
	      if keyword_set(cut256x256) then begin
		datacube = fltarr(nx,ny,nframes+sframes)
		datacube[*,*,0:nframes-1] = *cubecal
		if (s[1] gt nx || s[2] gt ny) then $
	          datacube[*,*,nframes:nframes+sframes-1] = (*data)[border+fix((resize-1)*nx)/2:border+fix((resize-1)*nx)/2+nx-1,  $
                                                                    border+fix((resize-1)*ny)/2:border+fix((resize-1)*ny)/2+ny-1] $
		else datacube[*,*,nframes:nframes+sframes-1] = *data
	      endif else begin
		datacube = fltarr(newnx,newny,nframes+sframes)
		datacube[*,*,0:nframes-1] = *cubecal
		if (s[1] lt newnx || s[2] lt newny) then begin
		  newdata = replicate(0.,newnx,newny,sframes)
		  newdata[newnx/2-s[1]/2:newnx/2+s[1]/2-1,newny/2-s[2]/2:newny/2+s[2]/2-1,*] = *data
		  datacube[*,*,nframes:nframes+sframes-1] = newdata
		endif else begin
		  datacube[*,*,nframes:nframes+sframes-1] = *data
	    	endelse
	      endelse
	      *cubecal = datacube
	    endelse
	    nframes = nframes + sframes
	  endif
	endif
      endif else begin
	; save file
	fname = rootname + "_" + strlowcase((*calprods)(i).name) + ".fits"
	; add pathsave to fname if there is no full path in file name
	if strpos(fname,path_sep()) lt 0 then begin
	  drip_message,'saved calibration data file: '+fname  
	  fname = self.pathcalsave + fname
	endif else begin
	  drip_message,'saved calibration data file: '+strmid(fname,strpos(fname,path_sep())+1,strlen(fname))
	endelse
	fits_write,fname,*data,*self.basehead      
      endelse
    endfor    
    
    if (output eq 'BOTHCUBE' || output eq 'CALCUBE') then begin
      ; save file
      fname = rootname + "_allcal.fits"
      ; add pathsave to fname if there is no full path in file name
      if strpos(fname,path_sep()) lt 0 then begin
	drip_message,'saved calibration data file: '+fname  
	fname = self.pathcalsave + fname
      endif else begin
	drip_message,'saved calibration data file: '+strmid(fname,strpos(fname,path_sep())+1,strlen(fname))
      endelse
      
      ; Create smaller cube since all the cal images are 256x256
      if keyword_set(cut256x256) eq 0 then begin
        writecalcube = (*cubecal)[newnx/2-nx/2:newnx/2+nx/2-1,newny/2-ny/2:newny/2+ny/2-1,*]
      endif else begin
        writecalcube = *cubecal      
      endelse
      
      ; Add information about what image is in each plane
      writecalhead = *self.basehead
      for i=0,n_elements(*calprods)-1 do begin
	printplane = 1
	if i eq 0 then begin
	  if calframes(0) eq 0 then rangecal = '0' $
	  else rangecal = '0-'+strtrim(calframes(0),1) 
	  if calframes(0) lt 0 then printplane = 0
	endif else begin
	  if calframes(i) eq calframes(i-1)+1 then rangecal = strtrim(calframes(i),1) $
	  else rangecal = strtrim(calframes(i-1)+1,1)+'-'+strtrim(calframes(i),1)
	  if calframes(i) eq calframes(i-1) then printplane = 0
	endelse
	if printplane eq 1 then $
	  sxaddpar,writecalhead,'PL'+rangecal,(*calprods)(i).name
      endfor
      fits_write,fname,writecalcube,writecalhead
    endif
    if (output eq 'CUBE') then totalframes = totalframes + nframes
  endif
  
  ;save data for reduced products
  if ((*redprods)(0).pointer ne ptr_new()) then begin
    if (output eq 'CUBE' || output eq 'BOTHCUBE' || output eq 'REDCUBE') then begin
      cubered = ptr_new(/allocate_heap)
      nframes = 0
    endif
    
    if (output ne 'CUBE') then begin
      if strpos(rootname,path_sep()) lt 0 then begin
        drip_message,'Directory for reduced data file: ' 
        drip_message,'      '+self.pathsave 
      endif else begin
        drip_message,'Directory for reduced data file: '
        drip_message,'      '+strmid(rootname,0,strpos(rootname,path_sep()))
      endelse
    endif 
    
    redframes = intarr(n_elements(*redprods))
    for i=0,n_elements(*redprods)-1 do begin
      data = (*redprods)(i).pointer
      if (output eq 'CUBE' || output eq 'BOTHCUBE' || output eq 'REDCUBE') then begin
	s=size(*data)
	sframes = 0
	if s[0] eq 2 then sframes = 1 $
	else if s[0] eq 3 then sframes = s[3]
	
	; complete calframes which will keep track of the number of frames 
	; for each type of data. This will be used to write what data 
	; is in each frame
	if i gt 0 then redframes(i)=redframes(i-1)+sframes $
	else redframes(0) = sframes-1	
	
	if sframes ne 0 then begin
	  if (s[1] ge nx && s[2] ge ny) then begin 
	    if nframes eq 0 then begin
	      if keyword_set(cut256x256) then begin
		;if (s[1] gt nx || s[2] gt ny) then *cubered = *data[border:border+nx-1,border:border+ny-1] $
		if (s[1] gt nx || s[2] gt ny) then *cubered = *data[border+fix((resize-1)*nx)/2:border+fix((resize-1)*nx)/2+nx-1,  $
                                                                    border+fix((resize-1)*ny)/2:border+fix((resize-1)*ny)/2+ny-1] $
		else *cubered = *data
	      endif else begin
		if (s[1] lt newnx || s[2] lt newny) then begin
		  newdata = replicate(0.,newnx,newny,sframes)
		  newdata[newnx/2-s[1]/2:newnx/2+s[1]/2-1,newny/2-s[2]/2:newny/2+s[2]/2-1,*] = *data
		  *cubered = newdata
		endif else begin
		  *cubered = *data
	    	endelse
	      endelse
	    endif else begin
	      if keyword_set(cut256x256) then begin
        	datacube = fltarr(nx,ny,nframes+sframes)
		datacube[*,*,0:nframes-1] = *cubered
		if (s[1] gt nx || s[2] gt ny) then $
	          $ ;datacube[*,*,nframes:nframes+sframes-1] = (*data)[border:border+nx-1,border:border+ny-1] $
	          datacube[*,*,nframes:nframes+sframes-1] = (*data)[border+fix((resize-1)*nx)/2:border+fix((resize-1)*nx)/2+nx-1,  $
                                                                    border+fix((resize-1)*ny)/2:border+fix((resize-1)*ny)/2+ny-1] $
		else datacube[*,*,nframes:nframes+sframes-1] = *data
	      endif else begin
		datacube = fltarr(newnx,newny,nframes+sframes)
		datacube[*,*,0:nframes-1] = *cubered
		if (s[1] lt newnx || s[2] lt newny) then begin
		  newdata = replicate(0.,newnx,newny,sframes)
		  newdata[newnx/2-s[1]/2:newnx/2+s[1]/2-1,newny/2-s[2]/2:newny/2+s[2]/2-1,*] = *data
		  datacube[*,*,nframes:nframes+sframes-1] = newdata
		endif else begin
		  datacube[*,*,nframes:nframes+sframes-1] = *data
	    	endelse
	      endelse
	      *cubered = datacube
	    endelse
	    nframes = nframes + sframes
	  endif
	endif
      endif else begin
	; save file
	fname = rootname + "_" + strlowcase((*redprods)(i).name) + ".fits"
	; add pathsave to fname if there is no full path in file name
	if strpos(fname,path_sep()) lt 0 then begin
	  drip_message,'saved reduced data file: '+fname  
	  fname = self.pathsave + fname
	endif else begin
	  drip_message,'saved reduced data file: '+strmid(fname,strpos(fname,path_sep())+1,strlen(fname))
        endelse

                                ; Extract processing HISTORY out of
                                ; self.basehead and add it to
                                ; self.header for saving intermediate
                                ; data products
        ;help,*self.basehead
        base_header = *self.basehead
        proc_hist=base_header(where(strmatch(*self.basehead, 'HISTORY*') eq 1))  ; Extract HISTORY keywords and data
        header_header = *self.header
        
        header_header = header_header[0:n_elements(header_header)-2]  ; strip END keyword off
        int_write_header = [header_header, proc_hist, 'END     ']          ; add HISTORY to the end of self.header and add END
        ;print,int_write_header
        
        if strmatch(fname,'*coadd*') eq 1 then int_write_header = *self.basehead  ; use base header for final coadded image(s)
	fits_write,fname,*data, int_write_header  
  
      endelse
    endfor    
    
    if (output eq 'BOTHCUBE' || output eq 'REDCUBE') then begin
      ; save file
      fname = rootname + "_allred.fits"
      ; add pathsave to fname if there is no full path in file name
      if strpos(fname,path_sep()) lt 0 then begin
	drip_message,'saved reduced data file: '+fname  
	fname = self.pathsave + fname
      endif else begin
	drip_message,'saved reduced data file: '+strmid(fname,strpos(fname,path_sep())+1,strlen(fname))
      endelse
            
      ; Add information about what image is in each plane

      writeredhead = *self.basehead

      for i=0,n_elements(*redprods)-1 do begin
	printplane = 1
	if i eq 0 then begin
	  if redframes(0) eq 0 then rangered = '0' $
	  else rangered = '0-'+strtrim(redframes(0),1) 
	  if redframes(0) lt 0 then printplane = 0
	endif else begin
	  if redframes(i) eq redframes(i-1)+1 then rangered = strtrim(redframes(i),1) $
	  else rangered = strtrim(redframes(i-1)+1,1)+'-'+strtrim(redframes(i),1)
	  if redframes(i) eq redframes(i-1) then printplane = 0
	endelse
	if printplane eq 1 then $
	  sxaddpar,writeredhead,'PL'+rangered,(*redprods)(i).name
      endfor
      fits_write,fname,*cubered,writeredhead
    endif
    if (output eq 'CUBE') then totalframes = totalframes + nframes
  endif
  
  if (output eq 'CUBE') then begin
    if keyword_set(cut256x256) then cubeall = fltarr(nx,ny,totalframes)  $
    else cubeall = fltarr(newnx,newny,totalframes)
    scal = size(*cubecal)
    if scal[0] eq 2 then cubeall(*,*,0) = *cubecal $
    else cubeall(*,*,0:scal[3]-1) = *cubecal 
    sred = size(*cubered)
    if sred[0] eq 2 then cubeall(*,*,totalframes-1) = *cubered $
    else cubeall(*,*,scal[3]:totalframes-1) = *cubered 
  
    ; save file
    fname = rootname + "_all.fits"
    ; add pathsave to fname if there is no full path in file name    
    if strpos(fname,path_sep()) lt 0 then begin
      drip_message,'Directory for data cube: '
      drip_message,'      '+self.pathsave
      drip_message,'saved calibration data file: '+fname  
      fname = self.pathsave + fname
    endif else begin
      drip_message,'Directory for data cube: '
      drip_message,'      '+strmid(rootname,0,strpos(rootname,path_sep()))
      drip_message,'saved data cube: '+strmid(fname,strpos(fname,path_sep())+1,strlen(fname))
    endelse

    writehead = *self.basehead

    for i=0,n_elements(*calprods)-1 do begin
      printplane = 1
      if i eq 0 then begin
        if calframes(0) eq 0 then rangecal = '0' $
        else rangecal = '0-'+strtrim(calframes(0),1) 
        if calframes(0) lt 0 then printplane = 0
      endif else begin
        if calframes(i) eq calframes(i-1)+1 then rangecal = strtrim(calframes(i),1) $
        else rangecal = strtrim(calframes(i-1)+1,1)+'-'+strtrim(calframes(i),1)
        if calframes(i) eq calframes(i-1) then printplane = 0
      endelse
      if printplane eq 1 then $
        sxaddpar,writehead,'PL'+rangecal,(*calprods)(i).name
    endfor
    redzero = calframes(n_elements(*calprods)-1)
    for i=0,n_elements(*redprods)-1 do begin
      printplane = 1
      if i eq 0 then begin
        if redzero eq 0 then begin
	  if redframes(0) eq 0 then rangered = strtrim(redzero,1) $
          else rangered = strtrim(redzero,1)+'-'+strtrim(redframes(0)+redzero,1) 
	endif else begin
	  if redframes(0) eq 0 then rangered = strtrim(redzero+1,1) $
          else rangered = strtrim(redzero+1,1)+'-'+strtrim(redframes(0)+redzero+1,1) 
	endelse
        if redframes(0) lt 0 then printplane = 0
      endif else begin
        if redzero eq 0 then begin
          if redframes(i) eq redframes(i-1)+1 then rangered = strtrim(redframes(i)+redzero,1) $
          else rangered = strtrim(redframes(i-1)+1+redzero,1)+'-'+strtrim(redframes(i)+redzero,1)
          if redframes(i) eq redframes(i-1) then printplane = 0
	endif else begin
          if redframes(i) eq redframes(i-1)+1 then rangered = strtrim(redframes(i)+redzero+1,1) $
          else rangered = strtrim(redframes(i-1)+2+redzero,1)+'-'+strtrim(redframes(i)+redzero+1,1)
          if redframes(i) eq redframes(i-1) then printplane = 0
	endelse
      endelse
      if printplane eq 1 then $
        sxaddpar,writehead,'PL'+rangered,(*redprods)(i).name
    endfor
    fits_write,fname,cubeall,writehead
  endif
    
  ; Free memory for cubecal and cubered
  if ((*calprods)(0).pointer ne ptr_new()) then begin
    if (output eq 'CUBE' || output eq 'BOTHCUBE' || output eq 'CALCUBE') then begin
      if cubecal ne ptr_new() then ptr_free,cubecal
    endif
  endif  
  if ((*redprods)(0).pointer ne ptr_new()) then begin
    if (output eq 'CUBE' || output eq 'BOTHCUBE' || output eq 'REDCUBE') then begin
      if cubered ne ptr_new() then ptr_free,cubered
    endif
  endif
  
  ; Also free memory for calprods and redprods
  ; I initially did not create a heap in the case of all
  ; but I changed that to take care of the merge case
  ; for c2nc2 mode
  ;k = where(names eq 'all')
  ;if k(0) eq -1 then begin
    if calprods ne ptr_new() then ptr_free,calprods
    if redprods ne ptr_new() then ptr_free,redprods
  ;endif
end


;******************************************************************************
;     FINDCAL - find support files (flat, dark)
;******************************************************************************

pro drip::findcal
; Searches current data directory for flatfield data file and chooses the
; most recent flat that has the same instrument configuration as the data
; to be reduced.
no_flat_data=0 ; 1 = 'No flat data found', 0 = 'flat data found'
fitsdir,file_names, keyvalue, self.pathcal, t=self.pathcal+'fitsdir.txt', $
  /nosize, keywords='INSTMODE, INSTCFGN, DATE-OBS, TIME-OBS, OBJECT'
icfg=drip_getpar(*self.basehead,'INSTCFGN')
print,'FILENAMES',where(keyvalue[*,0] eq 'FLAT' and keyvalue[*,1] eq icfg)
; find most recent FLAT files with INSTCFGN matching current data file
; first check if any flat files are present in the data directory

flats_exist=where(keyvalue[*,0] eq 'FLAT' and keyvalue[*,1] eq icfg,count)

if (n_elements(file_names) gt 0 and count gt 0) then begin
                                      
    flat_list=file_names(where(keyvalue[*,0] eq 'FLAT' and keyvalue[*,1] eq icfg))
    key_list1=keyvalue[where(keyvalue[*,0] eq 'FLAT' and keyvalue[*,1] eq icfg),*]
    ; Sort by date
    date_sort=sort(key_list1[*,2])
    ;print,date_sort
    key_list2=key_list1[date_sort,*]
    flat_list2=flat_list[date_sort,*]
    latest_date=key_list2[n_elements(key_list2[*,2])-1,2]
    ;drip_message,'latest date is '+latest_date
    ; Sort by time stamp
    time_sort=sort(key_list2[*,3])
    key_list3=key_list2[time_sort,*]
    flat_list3=flat_list2[time_sort,*]
    latest_time=key_list3[n_elements(key_list3[*,3])-1,3]
    latest_flat=flat_list3[n_elements(flat_list)-1]
    ;drip_message,'latest time is '+latest_time
    ;drip_message,'latest flat is '+latest_flat

    self.flatfile=latest_flat+'.fits'
    drip_message,'drip::findcal - using flatfield file: '+latest_flat+'.fits'
    print,'FLAT FILE is ',self.flatfile
endif else begin
    drip_message, 'drip::findcal - no flatfield data found'
    self.flatfile=''; 1 = 'No flat data found', 0 = 'flat data found'
endelse

end

;******************************************************************************
;     GETCAL - get support files (nonlin, flat, dark and bad)
;******************************************************************************

pro drip::getcal


; read linearity correction coefficient files (DETCHAN = 0 for SWC, 1 for LWC)  ;LIN
det_chan=fix(drip_getpar(*self.basehead,'DETCHAN'),type=2)

if det_chan eq 0 then camera='SWC' $
else camera = 'LWC'


;** read files and enter obs_id's
; read bad pixel map
self.badfile=drip_getpar(*self.basehead,'BADFILE'+camera,writename='BADFILE')
if self.badfile eq 'x' then begin
    ; no file found - set badmap to 0.0
    drip_message, 'drip::getcal - No Bad Pixel Map'
    *self.badmap=fltarr(256,256)
    (*self.badmap)[*,*]=0.0
endif else begin
    drip_message, 'Bad pixel mask file is '+self.badfile
    if strpos(self.badfile,self.pathcal) lt 0 then $
        self.badfile=self.pathcal+self.badfile
    *self.badmap=readfits(self.badfile,badhead,/noscale,/silent)
    if size(*self.badmap,/N_dimensions) gt 0 then begin
        ; get bad map obs_id and add to basehead
        bad_obs_id=sxpar(badhead,'obs_id')
        sxaddpar,*self.basehead,'BAD_OID',bad_obs_id
        self.parentn=self.parentn+1
        parstring='PARENT'+strtrim(string(self.parentn),2)
        sxaddpar,*self.basehead,parstring,bad_obs_id
    endif  else begin
        ; no valid data loaded - set badmap to 0.0
        drip_message, 'drip::getcal - Error Loading Bad Pixel Map = ' + $
          self.badfile
        *self.badmap=fltarr(256,256)
        (*self.badmap)[*,*]=0.0
    endelse
endelse
*self.badflags=*self.badmap
; read dark frames
self.darkfile=drip_getpar(*self.basehead,'DARKFILE'+camera,writename='DARKFILE')
if self.darkfile eq 'x' then begin
    ; no file found - set dark to 0.0
    drip_message, 'drip::getcal - No Dark Frames'
    *self.darks=fltarr(256,256)
    (*self.darks)[*,*]=0.0
endif else begin
    if strpos(self.darkfile,self.pathcal) lt 0 then $
        self.darkfile=self.pathcal+self.darkfile
    *self.darks=readfits(self.darkfile,darkhead,/noscale,/silent)
    if size(*self.darks,/N_dimensions) gt 0 then begin
        ; get darks obs_id and add to basehead
        dark_obs_id=sxpar(darkhead,'obs_id')
        sxaddpar,*self.basehead,'DARK_OID',dark_obs_id
        self.parentn=self.parentn+1
        parstring='PARENT'+strtrim(string(self.parentn),2)
        sxaddpar,*self.basehead,parstring,dark_obs_id
    endif  else begin
        ; no valid data loaded - set dark to 0.0
        drip_message, 'drip::getcal - Error Loading Dark Frames = ' + $
          self.darkfile
        *self.darks=fltarr(256,256)
        (*self.darks)[*,*,*]=0.0
    endelse
endelse

no_linearity_data = 0 ; 1 = 'No linearity data found', 0 = 'linearity data found'
;det_chan=fix(sxpar(*self.basehead,'DETCHAN'))
if det_chan eq 0 then begin
    self.linfile=self.pathcal+'SWC_linearity_coeff.fits'
    *self.lincor=readfits(self.linfile,/noscale,/silent)
endif else begin
    if det_chan eq 1 then begin
    self.linfile=self.pathcal+'LWC_linearity_coeff.fits'
    *self.lincor=readfits(self.linfile,/noscale,/silent)
endif else begin
    ; no valid data loaded - set linearity correction to 1.0 (no correction)
    drip_message, 'drip::getcal - Error Loading linearity coeff file = ' + self.linfile
    no_linearity_data = 1
    ; Need a graceful way to abort non-lin correction
endelse
endelse
s=size(*self.lincor)
if s[0] lt 3 then begin
    drip_message, 'drip::getcal - Error Loading linearity coeff file = ' + self.linfile
    no_linearity_data = 1
    *self.lincor=fltarr(256,256)+1.0    
endif

; read flat fields

if self.flatfile ne '' then no_flat_data=0
if self.flatfile eq '' then begin
    self.flatfile=drip_getpar(*self.basehead,'FLATFILE'+camera,writename='FLATFILE')
    no_flat_data=0 ; 1 = 'No flat data found', 0 = 'flat data found'
endif
if self.flatfile eq 'x' then begin
    ; no file found - set flat to 1.0
    drip_message, 'drip::getcal - No Flat Frames'
    *self.flats=fltarr(256,256)
    dimx = 256
    dimy = 256
    (*self.flats)[*,*]=1.0
    no_flat_data=1
endif else begin
    ; Add pathcal if it is not present in the filename
    if strpos(self.flatfile,self.pathcal) lt 0 then $
        self.flatfile=self.pathcal+self.flatfile
    *self.flats=readfits(self.flatfile,flathead,/noscale,/silent)
    if size(*self.flats,/N_dimensions) gt 0 then begin
        ; get flat obs_id and add to basehead
        flat_obs_id=sxpar(flathead,'obs_id')
        sxaddpar,*self.basehead,'FLAT_OID',flat_obs_id
        self.parentn=self.parentn+1
        parstring='PARENT'+strtrim(string(self.parentn),2)
        sxaddpar,*self.basehead,parstring,flat_obs_id
	dimx = s[1]
	dimy = s[2]
    endif else begin
        ; no valid data loaded - set flat pixels values to 1.0
        drip_message, 'drip::getcal - Error Loading Flat Frames = ' + $
          self.flatfile
        drip_message, 'drip::getcal - Skipping flatfield correction'
        *self.flats=fltarr(256,256)
	dimx = 256
	dimy = 256
        (*self.flats)[*,*]=1.0
        no_flat_data=1
    endelse
endelse
; Read section of images for background level calculation
self.imglinsection = read_section(dimx,dimy,*self.basehead)
drip_message, 'drip::getcal - Section ['+strtrim(self.imglinsection[2],1)+'x'+strtrim(self.imglinsection[3],1)+'] is centered at '+strtrim(self.imglinsection[0],1)+'x'+strtrim(self.imglinsection[0],1)

;** make master flat
; make darksum
*self.cleaneddarks=drip_clean(*self.darks,*self.badmap,*self.basehead)
drip_message,'drip::getcal - done cleaning darks'
s=size(*self.cleaneddarks)
if s[0] gt 2 then darksum=total(*self.cleaneddarks,3)/s[3] else darksum=*self.cleaneddarks
*self.darksum=darksum

; make flatsum
*self.cleanedflats=drip_clean(*self.flats,*self.badmap,*self.basehead)
drip_message,'drip::getcal - done cleaning flats'
;apply droop correction
*self.droopedflats=drip_droop(*self.cleanedflats,*self.basehead)
drip_message,'drip::getcal - done removing droop of flats'
;apply image non-linearity correction
siglevflat = drip_background(*self.droopedflats,self.imglinsection)
*self.imglinearizedflats=drip_imgnonlin(*self.droopedflats,*self.basehead, siglev=siglevflat)
drip_message,'drip::getcal - done correcting image linearity of flats'
;Apply ordermask to flatfield: used for grism mode (if grism mode
;detected) to ignore inter-order pixels when calculating median values
;for the master flatfield image. If imaging mode, then used the entire
;array.

filter=drip_getpar(*self.basehead,'FILT2_S')
;filter=sxpar(*self.basehead,'FILT2_S')
if ((filter eq 'grism2') OR (filter eq 'grism4')) then begin
    ;get the order mask
    fname=drip_getpar(*self.basehead,'MASKFILE')
    ;fname=sxpar(*self.basehead,'MASKFILE')
    ordermask=readfits(self.pathcal+fname,/silent)
endif else begin
   ordermask=dblarr(256,256)+1
endelse
       
;Build master flatfield image. Flat files are 4-plane fits data cubes
; MASTERFLAT IS DARK-SUBTRACTED
;Planes are: HOT (0), HOT (1), COLD (2), COLD (3)
s=size(*self.imglinearizedflats)
if s[0] eq 2 then begin
    flatdiff=*self.imglinearizedflats;    -*self.darksum
    master=flatdiff/median(flatdiff[where(ordermask eq 1)])
    if (no_flat_data eq 0) then master[where(ordermask eq 0)]=1.
endif
if s[0] eq 3 then begin
    CASE 1 of 
        (s[3] eq 4): begin  
          ; First frame is the sum integrations on a warmer/brighter source
          hflat=((*self.imglinearizedflats)[*,*,0]+(*self.imglinearizedflats)[*,*,1])/2
          ; Second frame is the sum integrations on a cooler/fainter source 
          lflat=((*self.imglinearizedflats)[*,*,2]+(*self.imglinearizedflats)[*,*,3])/2
          flatdiff=hflat-lflat
          master=flatdiff/median(flatdiff[where(ordermask eq 1)]) ; normalize
          if ((filter eq 'grism2') OR (filter eq 'grism4')) then master[where(ordermask eq 0)]=1.
          end
          
       (s[3] eq 2): begin
          ; First frame is integration on a warmer/brighter source
          hflat=((*self.imglinearizedflats)[*,*,0])
          ; Second frame is integration on a cooler/fainter source 
          lflat=((*self.imglinearizedflats)[*,*,1])
          flatdiff=hflat-lflat
          master=flatdiff/median(flatdiff[where(ordermask eq 1)]) ; normalize
          if ((filter eq 'grism2') OR (filter eq 'grism4')) then master[where(ordermask eq 0)]=1.
          end
       (s[3] eq 3) OR (s[3] gt 4): begin  ; Assume planes are images of same source
          flatsum=total(*self.imglinearizedflats,3)
          ; make masterflat: darksub and normalize
          flatdiff=flatsum    ;flatsum-*self.darksum
          master=flatdiff/median(flatdiff[where(ordermask eq 1)])
          if ((filter eq 'grism2') OR (filter eq 'grism4')) then master[where(ordermask eq 0)]=1.
          end
    ENDCASE
endif

;apply non-linearity correction...                        LIN
if no_flat_data eq 0 then begin 
    *self.masterflat=(drip_nonlin(master, *self.lincor))
    drip_message,'drip::getcal - done correcting pixel non-linearity of flats'
    ;else *self.masterflat=master   ; ...unless no flat data found
endif

; Kill any zeros generated in the process by hot pixels
; Note that this may not be the best method. An alternative is to
; replace zero values with the mdian of the surrounding pixels using
; MASKINTERP.PRO as we do to clean bad pixels in DRIP_CLEAN.PRO

if no_flat_data eq 0 then begin
    mzeros=where(master le 0)
    print,'MZEROS ',mzeros
    if (total(mzeros) ne -1) then begin
        master(where(master le 0))=median(master)  
    endif
    *self.masterflat=master
endif

; Interpolate over "jailbar" array pattern noise for SWC
; Get mask for "jailbar" array pattern noise in flats then
; use maskinterp.pro to interpolate and remove pattern

if (det_chan eq 0 and drip_getpar(*self.basehead,'MASKFLT') eq 'Y') then begin
    fname=drip_getpar(*self.basehead,'FMASKFILE')
    jbmask=readfits(self.pathcal+fname,/silent)
    *self.masterflat=maskinterp(*self.masterflat,jbmask,1,10,"csplinterp")
    drip_message,'drip::getcal - done with jailbar clean in flats'
endif

;create pinhole model for distortion correction
;read info from dripconfig and update basehead
pin_npts_read=drip_getpar(*self.basehead,'PIN_NPTS')
pin_npts=fix(strsplit(pin_npts_read,"[],",/extract))
sxaddpar,*self.basehead,'PIN_NPTS',pin_npts_read

spx_read=drip_getpar(*self.basehead,'PIN_SPX')
spx=fix(strsplit(spx_read,'[],',/extract))
spy_read=drip_getpar(*self.basehead,'PIN_SPY')
spy=fix(strsplit(spy_read,'[],',/extract))
if n_elements(spx) eq n_elements(spy) then begin
  sxaddpar,*self.basehead,'PIN_SPX',spx_read
  sxaddpar,*self.basehead,'PIN_SPY',spy_read
endif else begin
  spx = -1
  spy = -1
endelse

; read pinhole_file from dripconfig
pinhole_file=drip_getpar(*self.basehead,'PINHOLE_FILE')
pinhole_file=self.pathcal+pinhole_file
sxaddpar,*self.basehead,'PINHOLE_FILE',pinhole_file

; Here I assume that the size will be the same as the flats
; which it is very likely
; An improvement could be to read the size from basehead but
; I wouldn't bother too much with this
pinpos = distcorr_model(pinhole_file,NXIMG=s[1], NYIMG=s[2], $
                        NXPTS=pin_npts[0], NYPTS=pin_npts[1], SPX=spx, SPY=spy,BASEHEAD=*self.basehead)  ;,/viewpin)

; update header with xpos,ypos,xmod, ymod distcorr_model returns something right
if pinpos(0) eq -1 then begin
  drip_message, 'drip::getcal - Error calculating distortion correction model'
endif else begin
  *self.pinpos = pinpos
endelse
drip_message,'drip::getcal - done calculating distortion correction'

; set output file obs_id and pipe version
obs_id=drip_getpar(*self.basehead,'OBS_ID')
new_obs_id='P_'+obs_id
sxaddpar,*self.basehead,'OBS_ID',new_obs_id
sxaddpar,*self.basehead,'PIPEVERS','Forcast_Drip_1.0'

end

;****************************************************************************
;     STEPBACK - ignore last reduced image or list of images
;****************************************************************************

pro drip::stepback, list=list

print,'drip_stepback - started'

if keyword_set(list) eq 0 then begin
  if (size(*self.lastcoadd))[0] gt 0 then begin
    ; replace previous data and clean lastcoadd
    *self.coadded=*self.lastcoadd
    (*self.lastcoadd)=0
    *self.basehead = *self.lastbasehead
    *self.lastbasehead = *self.header
    
    ; lower self.n
    self.n=self.n-1
    print,'drip_stepback - done'
  endif else print,'drip_stepback - no previous data saved'
endif else begin
  if (size(*self.lastlistcoadd))[0] gt 0 then begin
    ; replace previous data and clean lastlistcoadd
    *self.coadded=*self.lastlistcoadd
    (*self.lastlistcoadd)=0
    *self.basehead = *self.lastlistbasehead
    *self.lastlistbasehead = *self.header
    
    ; lower self.n
    self.n=self.lastlistN
    print,'drip_stepback, /list - done'
  endif else print,'drip_stepback, /list - no previous data saved'
endelse
end

;****************************************************************************
;     REDUCE - reduce current data
;****************************************************************************

pro drip::reduce
; This procedure is only used if there is no INSTMODE specified
; that has its own pipeline definition (e.g. c2n__define.pro for INSTMODE='C2N')

print, 'NO INSTMODE specified to DRIP, using generic reduction steps'

; clean
*self.cleaned=drip_clean(*self.data,*self.badmap,*self.basehead)
; droop
*self.drooped=drip_droop(*self.cleaned,*self.basehead) 
; Calculate signal level
siglev = drip_background(*self.drooped,self.imglinsection,header=*self.basehead)
; image non-linerity
*self.imglinearized=drip_imgnonlin(*self.drooped,*self.basehead) 
; nonlin
*self.linearized=drip_nonlin(*self.imglinearized,*self.lincor)     ;LIN
; flat
*self.flatted=drip_flat(*self.linearized,*self.masterflat,*self.darksum)    ;LIN
; stack
*self.stacked=drip_stack(*self.flatted,*self.header)
; Remove third axis from header since we only have one frame after stacking
  sxaddpar, *self.basehead, 'NAXIS', 2
  sxdelpar, *self.basehead, 'NAXIS3'
; undistort
;*self.undistorted=drip_undistort(*self.stacked,*self.header,*self.basehead)
  *self.undistorted=drip_undistort(*self.stacked,*self.basehead,PINPOS=*self.pinpos)
; merge
*self.merged=drip_merge(*self.undistorted,*self.flatted,*self.header)
; coadd
if self.n gt 0 then begin
    *self.coadded=*self.coadded+*self.merged 
endif else begin
    *self.coadded=*self.merged     
endelse
; create readme
;print,'REDUCE FINISHED'
end

;****************************************************************************
;     RUN - reduces filelist
;****************************************************************************

pro drip::run, filelist, resetlist=resetlist

; check validity of filelist
lists=size(filelist)
if lists[1+lists[0]] ne 7 then begin
    drip_message, 'drip::run - must specify file name(s)'
    return
endif
; make file list if only 1 file given
if lists[0] eq 0 then begin
    filelist=[filelist]
    lists=size(filelist)
endif
; getcal if necessary
; Use findcal if keyword 'find_cal' is set to 'y' in dripconfig
if drip_getpar(*self.basehead,'FIND_CAL') eq 'Y' then begin
    drip_message, 'drip::run - Using findcal to find flat data'
    self->findcal
endif

if self.n eq 0 then self->getcal

; Write level_2 product in the header
sxaddpar,*self.basehead,'PROCSTAT','LEVEL_2'

; Store current configuration to be able to come back to the configuration
; before reducing the current filelist
if keyword_set(resetlist) then begin
  self.lastlistN = self.n
  *self.lastlistcoadd=*self.coadded
  *self.lastlistbasehead=*self.basehead
endif

; loop over files
for filei=0,lists[1]-1 do begin
    reduce_this_file=1
    self.filename=filelist[filei]
    ; load file and store lastheader
    if self.n gt 0 then *self.lastheader = *self.header
    data=readfits(self.filename,*self.header,/noscale,/silent)
    ; check data reliability
    if size(data, /n_dimen) lt 2 then begin
        drip_message, 'drip::run - nonvalid input file: '+ self.filename
        reduce_this_file=0
    endif
    ; check file mode
    if reduce_this_file gt 0 then begin
        mode=drip_getpar(*self.header,'INSTMODE')
        if mode eq 'x' then begin
            drip_message, $
              'drip::run - '+self.filename+' must have instmode keyword'
            reduce_this_file=0
        endif else begin
            ; Modify mode if C2 and C2NC2 flag exists
	    if mode eq 'C2' then begin
	      if (uint(sxpar(*self.header, 'C2NC2')) eq 1) then mode = 'C2NC2'
	    endif
	    if self.mode ne mode then begin
                drip_message, ['drip::run - file=' + self.filename , $
                  '   does not have current mode=' + self.mode, $
                  '   file ignored'], /fatal
                reduce_this_file=0
            endif
        endelse
    endif
    ; reduce data
    if reduce_this_file gt 0 then begin
        namepos=strpos(self.filename,path_sep(),/reverse_search)
        if namepos ge 0  then $
	  sxaddpar,*self.basehead,'HISTORY','FILE: '+strmid(self.filename,namepos+1,strlen(self.filename)) $
	else sxaddpar,*self.basehead,'HISTORY','FILE: '+self.filename
	*self.data=data
        ; reduce but if its first file copy basehead->head then ->basehead
        ;      to insure that keywords configured are replaced in
        ;      basehead (which is saved)
	; This is weird!! We need to check why we do this for n=0
	; In fact, I will comment those lines because it screw up the History of
        ; for the first image.
        if self.n gt 0 then begin
            *self.lastcoadd=*self.coadded
	    *self.lastbasehead=*self.basehead
            self->reduce
        endif else begin
            ;*self.header=*self.basehead
	    *self.lastheader = *self.header
            self->reduce
            ;*self.basehead=*self.header
            *self.lastcoadd=*self.coadded
            (*self.lastcoadd)[*,*]=0.0
	    
	    ; also update lastlist variables for this particular case self.n=0
            *self.lastbasehead=*self.basehead
            *self.lastlistbasehead=*self.basehead
            *self.lastlistcoadd=*self.coadded
            (*self.lastlistcoadd)[*,*]=0.0	   
	    self.lastlistN = 0
        endelse
        ; update settings
        self.n++
        self.parentn++
        parstring='PARENT'+strtrim(string(self.parentn),2)
        sxaddpar,*self.basehead,parstring,sxpar(*self.header,'OBS_ID')
    endif
endfor
end

;****************************************************************************
;     CLEANUP - Destroy pointer heap variables.
;****************************************************************************

pro drip::cleanup

; cleanup data
ptr_free, self.header
ptr_free, self.lastheader
ptr_free, self.basehead
ptr_free, self.lastbasehead
ptr_free, self.lastlistbasehead
ptr_free, self.data
ptr_free, self.cleaned
ptr_free, self.drooped
ptr_free, self.imglinearized
ptr_free, self.badflags
ptr_free, self.linearized   ;LIN
ptr_free, self.flatted
ptr_free, self.posdata
ptr_free, self.chopsub
ptr_free, self.nodsub
ptr_free, self.stacked
ptr_free, self.undistorted
ptr_free, self.merged
ptr_free, self.mergestack ; MERGESTACK
ptr_free, self.ditherp
ptr_free, self.coadded
ptr_free, self.coadded_rot
ptr_free, self.lastcoadd
ptr_free, self.lastlistcoadd
ptr_free, self.lastmergestack 
ptr_free, self.lastditherp
ptr_free, self.lastlistmergestack 
ptr_free, self.lastlistditherp
ptr_free, self.badmap
ptr_free, self.darks
ptr_free, self.flats
ptr_free, self.lincor   ;LIN
ptr_free, self.cleaneddarks
ptr_free, self.darksum
ptr_free, self.pinpos
ptr_free, self.cleanedflats
ptr_free, self.droopedflats
ptr_free, self.imglinearizedflats
ptr_free, self.masterflat

ptr_free,self.rawlist
ptr_free,self.reducedlist
ptr_free,self.callist

end

;****************************************************************************
;     INIT - Initialize structure fields.
;****************************************************************************

function drip::init, filelist

; initialize data
self.header=ptr_new(/allocate_heap)
*self.header=-1
self.lastheader=ptr_new(/allocate_heap)
*self.lastheader=-1
self.basehead=ptr_new(/allocate_heap)
*self.basehead=-1
self.lastbasehead=ptr_new(/allocate_heap)
*self.lastbasehead=-1
self.lastlistbasehead=ptr_new(/allocate_heap)
*self.lastlistbasehead=-1
self.data=ptr_new(/allocate_heap)
*self.data=-1
self.cleaned=ptr_new(/allocate_heap)
*self.cleaned=-1
self.drooped=ptr_new(/allocate_heap)
*self.drooped=-1
self.imglinearized=ptr_new(/allocate_heap)
*self.imglinearized=-1
self.badflags=ptr_new(/allocate_heap)
*self.badflags=-1
self.linearized=ptr_new(/allocate_heap)  ;LIN
*self.linearized=-1
self.flatted=ptr_new(/allocate_heap)
*self.flatted=-1
self.posdata=ptr_new(/allocate_heap)
*self.posdata=-1
self.chopsub=ptr_new(/allocate_heap)
*self.chopsub=-1
self.nodsub=ptr_new(/allocate_heap)
*self.nodsub=-1
self.stacked=ptr_new(/allocate_heap)
*self.stacked=-1
self.undistorted=ptr_new(/allocate_heap)
*self.undistorted=-1
self.merged=ptr_new(/allocate_heap)
*self.merged=-1
self.mergestack=ptr_new(/allocate_heap) ; MERGESTACK
*self.mergestack=-1
self.ditherp=ptr_new(/allocate_heap) 
*self.ditherp=-1
self.coadded=ptr_new(/allocate_heap)
*self.coadded=fltarr(256,256)
self.coadded_rot=ptr_new(/allocate_heap)
*self.coadded_rot=-1
self.lastcoadd=ptr_new(/allocate_heap)
*self.lastcoadd=-1
self.lastlistcoadd=ptr_new(/allocate_heap)
*self.lastlistcoadd=-1
self.lastmergestack=ptr_new(/allocate_heap) 
*self.lastmergestack=-1
self.lastditherp=ptr_new(/allocate_heap) 
*self.lastditherp=-1
self.lastlistmergestack=ptr_new(/allocate_heap) 
*self.lastlistmergestack=-1
self.lastlistditherp=ptr_new(/allocate_heap) 
*self.lastlistditherp=-1
self.badmap=ptr_new(/allocate_heap)
*self.badmap=-1
self.darks=ptr_new(/allocate_heap)
*self.darks=-1
self.flats=ptr_new(/allocate_heap)
*self.flats=-1
self.lincor=ptr_new(/allocate_heap)     ;LIN
*self.lincor=-1
self.cleaneddarks=ptr_new(/allocate_heap)
*self.cleaneddarks=-1
self.darksum=ptr_new(/allocate_heap)
*self.darksum=-1
self.pinpos=ptr_new(/allocate_heap)
*self.pinpos=-1
self.cleanedflats=ptr_new(/allocate_heap)
*self.cleanedflats=-1
self.droopedflats=ptr_new(/allocate_heap)
*self.droopedflats=-1
self.imglinearizedflats=ptr_new(/allocate_heap)
*self.imglinearizedflats=-1
self.masterflat=ptr_new(/allocate_heap)
*self.masterflat=-1

; complete raw, reduced and calibration array definitions
; This should be improvable but I do not see now how to write the information
; of the pointer in dripconf. 
self.rawlist=ptr_new(/allocate_heap)
self.reducedlist=ptr_new(/allocate_heap)
self.callist=ptr_new(/allocate_heap)
*(self.rawlist) = [{name:"RAW",pointer:self.data}]
*(self.reducedlist) = [{name:"CLEANED",pointer:self.cleaned}, $
               {name:"DROOPED",pointer:self.drooped}, $
               {name:"IMGLINEARIZED",pointer:self.imglinearized}, $
	       {name:"LINEARIZED",pointer:self.linearized}, $
	       {name:"FLATTED",pointer:self.flatted}, $
               {name:"STACKED",pointer:self.stacked}, $
	       {name:"UNDISTORTED",pointer:self.undistorted}, $
	       {name:"MERGED",pointer:self.merged}, $
	       {name:"COADDED",pointer:self.coadded}, $
	       {name:"COADDED_ROT",pointer:self.coadded_rot}]
*(self.callist) = [{name:"BADMAP",pointer:self.badmap}, $
           {name:"DARKS",pointer:self.darks},  $
	   {name:"FLATS",pointer:self.flats},  $
	   {name:"LINEAR_CORRECTION",pointer:self.lincor}, $
	   {name:"CLEANED_DARKS",pointer:self.cleaneddarks}, $
           {name:"DARKSUM",pointer:self.darksum}, $
	   {name:"CLEANED_FLATS",pointer:self.cleanedflats}, $
	   {name:"DROOPED_FLATS",pointer:self.droopedflats}, $
	   {name:"IMGLINEARIZED_FLATS",pointer:self.imglinearizedflats}, $
	   {name:"MASTERFLAT",pointer:self.masterflat}]

; check validity of filelist
lists=size(filelist)
if lists[1+lists[0]] ne 7 then begin
    drip_message, 'drip::init - must specify valid file name(s)'
    return, 0
endif
; get filename
if lists[0] gt 0 then file=filelist[0] else file=filelist
; set path and filename
pathend=strpos(file,'/',/reverse_search)
if pathend eq -1 then pathend=strpos(file,'\',/reverse_search)
pathend=pathend+1
self.pathload=strmid(file,0,pathend)
self.pathsave=self.pathload
self.pathcalsave=self.pathload
self.filename=strmid(file,pathend)
; load drip configuration file necessary
common drip_config_info, dripconf

if total(size(dripconf)) eq 0 then drip_config_load
; get base header
fits_read, file, null, *self.basehead, /header

; get working directory and store path for calibration
workpath = drip_getpar(*self.basehead,'CALDATA')
if workpath eq 'x' then begin
  self.pathcal=self.pathload+"../../data/" $
endif else begin
  self.pathcal=workpath    ;+"../data/"
endelse

if file_test(self.pathcal, /directory) eq 0 then begin
  drip_message,'drip::init - calibration path ('+self.pathcal+') does not exist'
  return, 0
endif

; get mode
self.mode=drip_getpar(*self.basehead,'INSTMODE',/vital)
if self.mode eq 'C2' then begin
  if (uint(sxpar(*self.basehead, 'C2NC2')) eq 1) then $
    self.mode='C2NC2'
endif
	  
; if no mode found exit
;    (this will never happen if drip_new is used as that procedure
;     always checks for INSTMODE first)
if self.mode eq 'x' then begin
    drip_message,'drip::init - No INSTMODE found - exiting',/fatal
    return, 0
endif
;success
return, 1
end

;****************************************************************************
;     DRIP__DEFINE - Define the DRIP class structure.
;****************************************************************************

pro drip__define

struct={drip, $
        ; admin variables
        mode:'', $                  ; FORCAST instrument mode
        n:0, $                      ; number of reduced files
        coaddN:0, $                 ; number of coadded images
        lastlistN:0, $              ; number of previous reduced files 
	                            ;          = self.n before calling run,filelist
	                            ;          = self.n-n_elements(filelist) after calling run,filelist
        readme:strarr(5), $         ; readme string
        parentn:0, $                ; counter for number of parent files
        ; file variables
        pathcal:'', $               ; path where the code is running. Define in dripconfig
        pathload:'', $              ; path for input files (from first file) which contains raw data
        pathcalsave:'', $           ; path to save calibration files produced by the pipeline
        pathsave:'', $              ; path to save files of reduced data
        filename:'', $              ; name of current file (w/o path)
        badfile:'', $               ; name of bad pix file
        darkfile:'', $              ; name of dark file
        linfile:'', $               ; name of linearity correction coeffs file   ;LIN
	imglinsection: [128,128,200,200], $ ;define section of the image used to calculate background
        flatfile:'', $              ; name of flats file
        header:ptr_new(), $         ; current file header
        lastheader:ptr_new(), $     ; file header of previous image (=header if not previous image)
        basehead:ptr_new(), $       ; output file header (=header of init file)
        lastbasehead:ptr_new(), $           ; output file header for previous image processing
        lastlistbasehead:ptr_new(), $       ; output file header for previous image list processing
	; Raw, Reduced and Calibration data product definition
	rawlist: ptr_new(), $       ; Array with pointers to the raw data
	reducedlist: ptr_new(), $   ; Array with pointers to the reduced data
	callist: ptr_new(), $       ; Array with pointers to the produced calibration data
        ; data variables
        data:ptr_new(), $           ; raw data
        cleaned:ptr_new(), $        ; cleaned data
	drooped:ptr_new(), $        ; data after correction of droop effects
	imglinearized:ptr_new(), $  ; data after correction of image linearity (not pixel linearity)
        badflags:ptr_new(), $       ; flags where data is bad
        linearized:ptr_new(), $     ; linearized data              ;LIN
        flatted:ptr_new(), $        ; flatfielded data
        posdata:ptr_new(), $        ; position data
        chopsub:ptr_new(), $        ; chop subtracted frames
        nodsub:ptr_new(), $         ; nod subtraced frames
        stacked:ptr_new(), $        ; stacked data
        undistorted:ptr_new(), $    ; undistored data
        merged:ptr_new(), $         ; merged data
        mergestack:ptr_new(), $     ; mergestack (for C2NC2 mode)
        ditherp:ptr_new(), $
        coadded:ptr_new(), $        ; coadded data
        coadded_rot:ptr_new(),$     ; coadded rotated
        lastcoadd:ptr_new(), $      ; previous coadded data
                                    ; (is set to 0 if stepback was called)
        lastlistcoadd:ptr_new(), $  ; previous coadded data used to step back a list of images  (vs one single image for lastcoadd)
        lastmergestack: ptr_new(),$ ; For steping back in c2nc2 mode
	lastditherp: ptr_new(), $   ; For steping back in c2nc2 mode
        lastlistmergestack: ptr_new(),$ ; For steping back in c2nc2 mode
	lastlistditherp: ptr_new(), $   ; For steping back in c2nc2 mode
	lastcoaddN:0,  $            ; It looks like crazy that I have to add this for stepping back c2nc2
	lastlistcoaddN:0, $         ; but it is true. It gets complicate when we come back to a previous set
	                            ; so I found that the easiest way is to just define those two extra parameter
	; support data varaiables
        badmap:ptr_new(), $         ; map of bad pixels
        darks:ptr_new(), $          ; dark frames
        lincor:ptr_new(), $         ; linarity correction coefficients     ;LIN
        flats:ptr_new(), $          ; flat frames
        cleaneddarks:ptr_new(), $   ; cleaned dark frames
        cleanedflats:ptr_new(), $   ; cleaned flat frames
        droopedflats:ptr_new(), $   ; drooped flat frames
        imglinearizedflats:ptr_new(), $   ; linearized flat frames
        masterflat:ptr_new(), $     ; master flat
        darksum:ptr_new(), $        ; sum of dark frames
	pinpos:ptr_new() $          ; position of the pinholes [Number of pinholes,Xpos model,Ypos model,Xpos measured,Ypos measured]
       }

end
