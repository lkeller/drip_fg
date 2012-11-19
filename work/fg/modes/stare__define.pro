; NAME:
;     STARE - Version .7.0
;
; PURPOSE:
;     Data Reduction Pipeline for stare mode
;
; CALLING SEQUENCE:
;     Obj=Obj_new('STARE', FILELIST)
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
;     of bad pixel map and flat fields. (Both can be files that are
;     filled with 0's)
;
; PROCEDURE:
;     run the individual pipeline procedures.
;
; MODIFICATION HISTORY:
;     Written by:  Marc Berthoud, Cornell University, October 2003
;                       Mostly Copied from TPC_DEFINE
;     Rewritten: Marc Berthoud, Cornell University, March 2005
;                     Mostly copied from tpc__define (the new one)
;     Modified:  Marc Berthoud, CU, March 2005
;                Renamed to STARE
;     Modified:  Marc Berthoud, Palomar, June 2005
;                use lastcoadded to store first frame, which is background
;                -> coadded is always last image - background (first image)
;     Modified:  Luke Keller, IC, January 2010
;                Added non-linearity correction 
;                
;     Modified:  Luke Keller, IC, July 2011
;                Grism mode version   
;     Modified:  Luke Keller, Ithaca College, May 2012
;                Updated to current DRIP version with GRISM capability                  
;

;******************************************************************************
;     REDUCE - Fills SELF structure pointer heap variables with proper values.
;******************************************************************************

pro stare::reduce

; clean
*self.cleaned=drip_clean(*self.data,*self.badmap, *self.basehead)
; droop
*self.drooped=drip_droop(*self.cleaned,*self.basehead) 
; Calculate signal level
siglev = drip_background(*self.drooped,self.imglinsection,header=*self.basehead)
; image non-linerity
*self.imglinearized=drip_imgnonlin(*self.drooped,*self.basehead) 
; nonlin
*self.linearized=drip_nonlin(*self.drooped,*self.lincor, *self.basehead)     ;LIN
; flat
*self.flatted=drip_flat(*self.linearized,*self.masterflat,*self.darksum)
; stack
if size(*self.flatted, /n_dimen) eq 3 then $
  *self.stacked=drip_stack(*self.flatted,*self.header) $
else *self.stacked=*self.flatted
; Remove third axis from header since we only have one frame after stacking
  sxaddpar, *self.basehead, 'NAXIS', 2
  sxdelpar, *self.basehead, 'NAXIS3'
*self.undistorted=*self.stacked  ; NO DISTORTION CORRECTION FOR GRISM MODES

sxaddpar, *self.basehead, 'HISTORY', 'Distortion correction NOT applied  for grism spectra'

;  *self.undistorted=drip_undistort(*self.stacked,*self.basehead,PINPOS=*self.pinpos)
;  

; EXTRACTION of 1-D spectrum from image
; Rotate array 90 degrees if single-order mode

;print,'GMODE', self.gmode
if (self.gmode gt 1) THEN BEGIN ;
    *self.undistorted=rot(*self.undistorted, 90.0)
endif

*self.extracted=drip_spextract(*self.undistorted, *self.basehead, self.gmode)
;print, 'extracted     ', *self.extracted
*self.allwave=(*self.extracted)[*,0]
*self.allflux=(*self.extracted)[*,1] 


; GUI: *self.extracted is a spectrum (1-D) that needs to be plotted and/or
; saved so we send it to cw_xlpot instead of display.
; 

; NO MERGE STEP FOR GRISM SPECTRA
*self.merged = *self.undistorted

;print, 'self.n=  :', self.n
; coadd
if self.n gt 0 then begin
    *self.coadded=drip_coadd(*self.extracted,*self.coadded, $
                             *self.header, *self.basehead)
    ; Turn off 2-D coadd, coadd 1-D spectra later
    ;*self.coadded=*self.merged
endif else begin
    *self.coadded=drip_coadd(*self.extracted,*self.coadded, $
                            *self.header, *self.basehead, /first)
    ; Turn off 2-D coadd, coadd 1-D spectra later
    ;*self.coadded=*self.merged
endelse
; create README
;o=(mode eq 1) ? 'on' : 'off'
o=''
self.readme=['pipeline: Stare Mode ' + o + ' chip DRiP', $ ;info lines
  'file: ' + self.filename, $
  'final image: undistorted', $
  'order: CLEAN, FLAT, STACK, COADD', $
  'notes: badmap from CLEAN, masterflat from FLAT']

;print,'Stare Mode FINISHED',self.n ;info
end

;******************************************************************************
;     STARE__DEFINE - Define the STARE class structure.
;******************************************************************************

pro stare__define  ;structure definition

struct={stare, $
      inherits drip} ; child object of drip object
end
