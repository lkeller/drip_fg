; NAME:
;     C3D - Version .7.0
;
; PURPOSE:
;     Data Reduction Pipeline for three position chop mode with dither
;
; CALLING SEQUENCE:
;     Obj=Obj_new('C3D', FILELIST)
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
; MODIFICATION HISTORY see drip__define for full history:
;     Rewritten: Marc Berthoud, CU, June 2004
;                Changed TPC object into a child object of the new
;                DRIP object (lots of code erased)
;     Created:   Marc Berthoud, CU, November 2004
;                Adapted from tpc__define.pro
;     Modified:  Marc Berthoud, CU, March 2005
;                Renamed to C3D
;

;******************************************************************************
;     RUN - Fills SELF structure pointer heap variables with proper values.
;******************************************************************************

pro c3d::reduce

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
*self.flatted=drip_flat(*self.linearized,*self.masterflat)
; stack
*self.stacked=drip_stack(*self.flatted,*self.header, posdata=*self.posdata,chopsub=*self.chopsub)
; Remove third axis from header since we only have one frame after stacking
  sxaddpar, *self.basehead, 'NAXIS', 2
  sxdelpar, *self.basehead, 'NAXIS3'
; undistort
;*self.undistorted=drip_undistort(*self.stacked,*self.header,*self.basehead)
  *self.undistorted=drip_undistort(*self.stacked,*self.basehead,PINPOS=*self.pinpos)
; merge
*self.merged=drip_merge(*self.undistorted,*self.header)
; coadd
if self.n gt 0 then begin
    *self.coadded=drip_coadd(*self.undistorted,*self.coadded, $
                             *self.header, *self.basehead)
endif else begin
    *self.coadded=drip_coadd(*self.undistorted,*self.coadded, $
                             *self.header, *self.basehead, /first)
endelse
; create README
;o=(mode eq 1) ? 'on' : 'off'
o=''
self.readme=['pipeline: 3 Position Chop with dither' + o + ' chip DRiP', $ ;info lines
  'file: ' + self.filename, $
  'final image: undistorted', $
  'order: CLEAN, FLAT, STACK, UNDISTORT, MERGE, COADD', $
  'notes: badmap from CLEAN, masterflat from FLAT']

print,'C3D ' + o + ' chip FINISHED' ;info
end

;******************************************************************************
;     C3D__DEFINE - Define the C3D class structure.
;******************************************************************************

pro c3d__define  ;structure definition

struct={c3d, $
      inherits drip} ; child object of drip object
end
