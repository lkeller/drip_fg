; NAME:
;     C2NC2 - Version .0.0
;
; PURPOSE:
;     Data Reduction Pipeline for two position chop with large offset to sky
;
; CALLING SEQUENCE:
;     Obj=Obj_new('C2NC2', FILELIST)
;     Structure=Obj->RUN(FILELIST)
;
; INPUTS:
;     FILELIST - Filename(s) of the fits file(s) containing data to be reduced.
;                May be a string array
;
; STRUCTURE:
;     (see drip__define)
;
; CALLED ROUTINES:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;     various components need to be developed. Data file must specify locations
;     of bad pixel map and flat fields.
;
; PROCEDURE:
;     run the individual pipeline procedures.
;
; MODIFICATION HISTORY:
;     Written by:  Luke, Ithaca College, May, 2011
;     Based on C2_-define.pro
;

;****************************************************************************
;     STEPBACK - ignore last reduced image or list of images
;****************************************************************************

pro drip::stepback, list=list

print,'drip_stepback (c2nc2) - started'

if keyword_set(list) eq 0 then begin
  if (size(*self.lastcoadd))[0] gt 0 then begin
    ; replace previous data and clean lastcoadd
    *self.coadded=*self.lastcoadd
    (*self.lastcoadd)=0
    *self.basehead = *self.lastbasehead
    *self.lastbasehead = *self.header
    *self.mergestack=*self.lastmergestack
    *self.ditherp=*self.lastditherp    
    
    ; lower self.n
    self.n=self.n-1
    self.coaddN = self.lastcoaddN
    print,'drip_stepback (c2nc2) - done'
  endif else print,'drip_stepback (c2nc2) - no previous data saved'
endif else begin
  if (size(*self.lastlistcoadd))[0] gt 0 then begin
    ; replace previous data and clean lastlistcoadd
    *self.coadded=*self.lastlistcoadd
    (*self.lastlistcoadd)=0
    *self.basehead = *self.lastlistbasehead
    *self.lastlistbasehead = *self.header
    *self.mergestack=*self.lastlistmergestack
    *self.ditherp=*self.lastlistditherp
    
    ; lower self.n
    self.n=self.lastlistN
    self.coaddN = self.lastlistcoaddN
    print,'drip_stepback (c2nc2), /list - done'
  endif else print,'drip_stepback (c2nc2), /list - no previous data saved'
endelse
  
end

;******************************************************************************
;     REDUCE - Fills SELF structure pointer heap variables with proper values.
;******************************************************************************

pro c2nc2::reduce

; clean
*self.cleaned=drip_clean(*self.data,*self.badmap,*self.header,basehead=*self.basehead)
; droop
*self.drooped=drip_droop(*self.cleaned,*self.basehead) 
; Calculate signal level
siglev = drip_background(*self.drooped,self.imglinsection,header=*self.basehead)
; image non-linerity
*self.imglinearized=drip_imgnonlin(*self.drooped,*self.basehead) 
; nonlin
*self.linearized=drip_nonlin(*self.drooped,*self.lincor)     ;LIN
; flat
*self.flatted=drip_flat(*self.linearized,*self.masterflat,*self.darksum,basehead=*self.basehead)

; stack
; REMOVED FLATFIELD CORRECTION 6/22/2011
*self.stacked=drip_stack(*self.linearized,*self.header, posdata=*self.posdata, $
                         chopsub=*self.chopsub,basehead=*self.basehead)
; Remove third axis from header since we only have one frame after stacking
sxaddpar, *self.basehead, 'NAXIS', 2
sxdelpar, *self.basehead, 'NAXIS3'
; undistort
;print,'************ UNDISTORTED *****************'
;*self.undistorted=drip_undistort(*self.stacked,*self.header,*self.basehead)

*self.undistorted=drip_undistort(*self.stacked,*self.basehead,PINPOS=*self.pinpos)

; ADDED *self.flatted for IMAGECORELLATION IN MERGE
;*self.merged=drip_merge(*self.undistorted,*self.flatted,*self.header)
;
; Add the new images to the mergestack for processing in coadd
; Here the new variable mergestack is filled. Need to arrange them
; ABABAB etc. for coadd.

;print,'************ COADD *****************'
new_ditherp = fix(drip_getpar(*self.header, 'DITHERP'))  ; Get dither position

if self.n eq 0 then begin ; Initialize ABAA sequence
    *self.mergestack = [[[*self.undistorted]]]
    *self.ditherp = intarr(fix(drip_getpar(*self.header, 'NDITHERS')))
    (*self.ditherp)[0] = new_ditherp
endif else begin
    max_ditherp = max(*self.ditherp)  ;[(n_elements(*self.ditherp)-1)] ; highest DITHERP so far
    if (new_ditherp gt max_ditherp) then begin
        ;sequence = ['A','B','A','A','B','A','A','B']
        *self.mergestack = [[[*self.mergestack]],[[*self.undistorted]]]
        (*self.ditherp)(where(*self.ditherp eq max_ditherp)+1) = new_ditherp ; [[*self.ditherp],[new_ditherp]]
        
        k1 = -1
        k2 = -1
        case new_ditherp of
            1: ; This never happens
            2: begin
                  k1 = where(*self.ditherp eq 1)
                  k2 = where(*self.ditherp eq 2)
		  useheader = *self.header ; *self.lastheader
               end         
            3: begin
                  k1 = where(*self.ditherp eq 3)
                  k2 = where(*self.ditherp eq 2)
		  useheader = *self.header
               end
            4: begin
                  ; Wait for the next frame
               end
            5: begin
                  k1 = where(*self.ditherp eq 4)
                  k2 = where(*self.ditherp eq 5)
		  useheader = *self.lastheader
               end
            6: begin
                  k1 = where(*self.ditherp eq 6)
                  k2 = where(*self.ditherp eq 5)
		  useheader = *self.header
               end
            7: ; Wait for next frame
            8: begin
                  k1 = where(*self.ditherp eq 7)
                  k2 = where(*self.ditherp eq 8)
		  useheader = *self.lastheader
               end
         endcase

        ;print,'Using header:  ',useheader

        if (k1(0) ne -1) and (k2(0) ne -1) then begin
            on_off_pair = [[[(*self.mergestack)[*,*,k1(0)]]],[[(*self.mergestack)[*,*,k2(0)]]]]
            self.coaddN+=1
         
            ;Subtract on_off_pair then spextract then coadd

            ; EXTRACTION of 1-D spectrum from image
            ; Rotate array 90 degrees if single-order mode


            if (self.gmode lt 2) THEN BEGIN ;
                on_off_pair=rot(on_off_pair, -90.0)
            endif

            sub = on_off_pair[*,*,1] - on_off_pair[*,*,0]
            *self.extracted=drip_spextract(sub, *self.basehead, self.gmode) ;header
            *self.allwave=(*self.extracted)[*,0]
            *self.allflux=(*self.extracted)[*,1]
    
            if self.coaddN eq 1 then begin
                *self.coadded=drip_coadd(*self.extracted,*self.coadded, $
                useheader, *self.basehead, /first)
            endif else begin
                *self.coadded=drip_coadd(*self.extracted,*self.coadded, $
                useheader, *self.basehead, n=self.coaddN)
            endelse
        endif       
    endif else begin  ; Initialize ABAA sequence
        *self.mergestack = *self.undistorted
        *self.ditherp = intarr(fix(drip_getpar(*self.header, 'NDITHERS')))
        (*self.ditherp)[0] = new_ditherp
        ; Need to account for orphan frame (e.g. A waiting for B)
    endelse
endelse    

; create README
self.readme=['pipeline: 2 Position Chop with large offset DRiP', $ ;info lines
  'file: ' + self.filename, $
  'final image: undistorted', $
  'order: CLEAN, NONLIN, FLAT, STACK, UNDISTORT, MERGE, COADD', $
  'notes: badmap from CLEAN, masterflat from FLAT']

print,'C2NC2 FINISHED' ;info
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
  self.lastlistcoaddN = self.coaddN
  *self.lastlistcoadd=*self.coadded
  *self.lastlistbasehead=*self.basehead
  if self.n gt 0 then begin
    *self.lastlistmergestack=*self.mergestack
    *self.lastlistditherp=*self.ditherp
  endif
endif

; loop over files
for filei=0,lists[1]-1 do begin
    reduce_this_file=1
    self.filename=filelist[filei]
    ; load file
    data=readfits(self.filename,*self.header,/noscale)
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
	    self.lastcoaddN = self.coaddN
	    *self.lastmergestack=*self.mergestack
	    *self.lastditherp=*self.ditherp
            self->reduce
        endif else begin
            ;*self.header=*self.basehead
	    self.lastcoaddN = 0
            self->reduce
            ;*self.basehead=*self.header
            *self.lastcoadd=*self.coadded
            (*self.lastcoadd)[*,*]=0.0
	    ; We do not need to define lastmergestack for n=0 since
	    ; we will start again from scratch in c2nc2 mode
	    ; in fact is the same thing for lastditherp 
	    ; so I will comment the following line to "save" prcessing time
	    ;*self.lastditherp = intarr(n_elements(*self.ditherp))
	    
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

;******************************************************************************
;     C2NC2__DEFINE - Define the C2NC2 class structure.
;******************************************************************************

pro c2nc2__define  ;structure definition

struct={c2nc2, $
      inherits drip} ; child object of drip object
end
