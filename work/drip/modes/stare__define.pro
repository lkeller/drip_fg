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

;******************************************************************************
;     RUN - Fills SELF structure pointer heap variables with proper values.
;******************************************************************************

pro stare::reduce

; clean
*self.cleaned=drip_clean(*self.data,*self.badmap)
; droop
*self.drooped=drip_droop(*self.cleaned,*self.basehead) 
; Calculate signal level
siglev = drip_background(*self.drooped,self.imglinsection,header=*self.basehead)
; image non-linerity
*self.imglinearized=drip_imgnonlin(*self.drooped,*self.basehead) 
; nonlin
*self.linearized=drip_nonlin(*self.drooped,*self.lincor)     ;LIN
; flat
*self.flatted=drip_flat(*self.linearized,*self.masterflat,*self.darksum)
; stack
if size(*self.flatted, /n_dimen) eq 3 then $
  *self.stacked=drip_stack(*self.flatted,*self.header) $
else *self.stacked=*self.flatted
; Remove third axis from header since we only have one frame after stacking
  sxaddpar, *self.basehead, 'NAXIS', 2
  sxdelpar, *self.basehead, 'NAXIS3'
; undistort
;*self.undistorted=*self.stacked
;*self.undistorted=drip_undistort(*self.stacked,*self.header,*self.basehead)
  *self.undistorted=drip_undistort(*self.stacked,*self.basehead,PINPOS=*self.pinpos)
; merge
*self.merged=*self.undistorted
; coadd
if self.n gt 0 then begin
    *self.coadded=drip_coadd(*self.merged,*self.coadded, $
                             *self.header, *self.basehead)
endif else begin
    *self.coadded=drip_coadd(*self.merged,*self.coadded, $
                             *self.header, *self.basehead, /first)
endelse
; create README
;o=(mode eq 1) ? 'on' : 'off'
o=''
self.readme=['pipeline: Stare Mode ' + o + ' chip DRiP', $ ;info lines
  'file: ' + self.filename, $
  'final image: undistorted', $
  'order: CLEAN, FLAT, STACK, COADD', $
  'notes: badmap from CLEAN, masterflat from FLAT']

print,'Stare Mode FINISHED',self.n ;info
end

;******************************************************************************
;     STARE__DEFINE - Define the STARE class structure.
;******************************************************************************

pro stare__define  ;structure definition

struct={stare, $
      inherits drip} ; child object of drip object
end
