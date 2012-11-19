; NAME:
;     DRIP_CLEAN - Version .7.0
;                  (former clean__define)
;
; PURPOSE:
;     Replaces bad pixels in an image with approximate values.
;
; CALLING SEQUENCE:
;     CLEANED=DRIP_CLEAN(DATA,BADMAP,BASEHEAD=BASEHEAD)
;
; INPUTS:
;     DATA - Array with bad pixels
;     BADMAP - Bad pixel map (bad pixels are zero)
;     BASEHEAD - Update header if keyword is set
;
; OUTPUTS:
;     CLEANED - see below
;
; SIDE EFFECTS:
;     None.
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Uses sigma_filter procedure to clean the image
;
; MODIFICATION HISTORY:
;   Written by:  Alfred Lee, Cornell University, 2001
;     Modified:   Alfred Lee, CU, June 6, 2002
;                 changed CLEAN into an object.
;     Modified:   Alfred Lee, CU, June 24, 2002
;                 created a RUN method to run the code as needed after a single
;                 initialization.
;     Modified:   Alfred Lee, CU, June 27, 2002
;                 changed bad pixel criterion into 3 standard deviations above
;                 or below the mean value of the image.
;     Modified:   Alfred Lee, CU, July 18, 2002
;                 enhanced error checking.
;     Modified:   Alfred Lee, CU, August 1, 2002
;                 updated to new architecture.  data now stored in a Data
;                 Manager object.
;     Modified:   Alfred Lee, CU, September 9, 2002
;                 changed GOTO statement into a REPEAT...UNTIL statement.
;     Rewritten:  Marc Berthoud, CU, July 2004
;                 rewrote form object to a one-line command
;                 most code erased (see clean__define v.5.2)
;                 uses maskinterp
;     Modified:   Marc Berthoud, CU, February, 2005
;                 added automatic detection of bad pixels
;                 (but commented it out)
;     Modified:   Luke Keller, Ithaca College, Bill Vacca, USRA June
;     2010
;                 added "jailbar' cleaning via FFT and median filter
;     Modified:   Miguel Charcos Llorens, USRA, July 2011
;                 Add keyword for updating fits header

;
;******************************************************************************
;     RUN - Fills SELF structure pointer heap variables with proper values.
;******************************************************************************

function drip_clean, data, badmap, header, basehead=basehead

; error check
s=size(data)
if (s[0] lt 2) or (s[0] gt 3) then begin
    drip_message, 'drip_clean - Must provide valid data array',/fatal
    if keyword_set(basehead) then $
      sxaddpar, basehead, 'HISTORY', 'Did not clean bad pixels (Invalid data)'
    return, data
endif
if size(badmap,/n_dimen) ne 2 then begin
    drip_message, $
      'drip_clean - Must provide valid bad pixel map - returning data'
    if keyword_set(basehead) then $
      sxaddpar, basehead, 'HISTORY', 'Did not clean bad pixels (Invalid mask)'
    return, data
endif
hs=size(header)
if (hs[0] ne 1) or (hs[2] ne 7) then begin
  drip_message, 'drip_clean - invalid header'
  if keyword_set(basehead) then $
    sxaddpar, basehead, 'HISTORY', 'Invalid header'
endif

; set variables
; Clean method to use for jailbar pattern removal
jbmethod=drip_getpar(header, 'JBCLEAN')
if keyword_set(basehead) then $
  sxaddpar, basehead, 'HISTORY', 'JBMETHOD is '+jbmethod

meddist=3 ; distance for taking median
sigfac=100.0 ; threshold for how many sigmas until a pixel is considered bad
; get array, do correction
if s[0] eq 2 then begin
    ;** one data frame
    ; make mask
    badmask=(badmap eq 0)
    ; clean
    cleaned=maskinterp(data,badmask,1,6,"plsfit")
endif else begin
    ;** numerous frames
    cleaned=fltarr(s[1],s[2],s[3],/nozero)
    jailbar=fltarr(s[1],s[2],s[3],/nozero)
    jcleaned=fltarr(s[1],s[2],s[3],/nozero)
    for i=0,s[3]-1 do begin
        ; SIGMA FILTER it
        ;cleaned[*,*,i]=sigma_filter(data[*,*,i],5, n_sigma=3)
        ; FIND BAD PIXELS
        ; find additional bad pixels
        datanow=data[*,*,i]
        ;datamed=median(datanow,meddist)
        ;sig=stdev(datanow-datamed)
        ;print,'  drip_clean: sig0=',sig
        ; get new sigma by eliminating really bad pixels
        ;badind=where((abs(datanow-datamed) gt sigfac*sig) or (badmap ne 0))
        ;if (size(badind))[0] gt 0 then begin
        ;    datamed[badind]=datanow[badind]
        ;    sig=stdev(datanow-datamed)
        ;endif
        ;print,'  drip_clean: sig1=',sig
        ; make mask
        ;badmask=(badmap eq 0)
        ;if (size(badind))[0] gt 0 then badmask[badind]=0
        ;badind=where(abs(datanow-datamed) gt sigfac*sig)
        ;if (size(badind))[0] gt 0 then badmask[badind]=0
        ; MASKINTERP it
        badmask=(badmap eq 0)
        ; clean bad pixels
        cleaned[*,*,i]=maskinterp(datanow,badmask,1,6,"plsfit")
        if jbmethod eq 'FFT' then begin
            ; remove "jailbar" array pattern noise
            jailbar[*,*,i]=cleaned[*,*,i]
            ; Generate FFT of image
            fft_jailbar = fft(jailbar[*,*,i])
            ; Create Mask in Fourier space
            indx = findgen(15)*16.0 + 16.0
            mask = complexarr(256,256) + 1.0
            mask[indx,*] = 0.0
            ; Apply Mask
            jcleaned[*,*,i] = abs(fft(fft_jailbar*mask,/INVERSE))
            cleaned[*,*,i] = jcleaned[*,*,i]
        endif
       ;if jbmethod eq 'MEDIAN' then begin
        ;    jailbar[*,*,i]=cleaned[*,*,i]
        ;    median_data=fltarr(256)
        ;    row_data=fltarr(16)
        ;    index=indgen(256/16)*16  ;index every 16th pixel in a row
        ;    for k=0,255 do begin
        ;        for j=0,15 do begin
        ;            jailbar(index+j,k,i)=median(jailbar(index+j,k,i))
        ;        endfor
        ;    endfor
        ;    cleaned[*,*,i] = cleaned[*,*,i]-jailbar[*,*,i]+median(cleaned[*,*,i])
            ;print,median(jailbar[*,*,i])
       ;endif
    endfor
endelse

if keyword_set(basehead) then $
  sxaddpar, basehead, 'HISTORY', 'Interpolate using maskinterp(data,mask,1,6,plsfit)'

; get rid of funky top line using data from line second to top
; - this has to be done because the top line is corrupted by clean
cleaned[*,255,*]=cleaned[*,254,*]
if keyword_set(basehead) then $
  sxaddpar, basehead, 'HISTORY', 'Remove corrupted line 255 - Replaced with 254'
  
return, cleaned
end
