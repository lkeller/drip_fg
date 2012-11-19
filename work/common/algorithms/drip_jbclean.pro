; NAME:
;     DRIP_JBCLEAN - Version .1.0
;
; PURPOSE:
;     Add raw frames
;
; CALLING SEQUENCE:
;     JBCLEANED=DRIP_JBCLEAN(DATA, HEADER, BASEHEADER=BASEHEADER)
;
; INPUTS:
;     DATA - Data to be stacked i.e. stacks in drip
;     HEADER - The fits header of the new input data file
;     BASEHEAD - If set the header is updated
;
; OUTPUTS:
;     JBCLEANED - The image after correction of Jail bar effects
;
; SIDE EFFECTS:
;     Featurs appear in the image for extended objects
;
; RESTRICTIONS:
;     None.
;
; PROCEDURE:
;     Filter the input data with a 16x16 box and remove the filtered image from 
;     the input image in order to remove most of the sources in the image. Then,
;     the jail bar is most of the features found in the subtracted image. We 
;     use this image to calculate the jail bar using median
;
; MODIFICATION HISTORY:
;   Written by:  Luke Keller, Ithaca College (?)
;   Modified: Miguel Charcos Llorens, USRA, July 2011
;               Add basehead to update headers of the products

function drip_jbclean, data, header, basehead=basehead

; Remove "jailbar" array pattern noise using FFT and
; to create a spatial filter
jbcleaned = data
jbmethod= drip_getpar(header, 'JBCLEAN')
;jailbar=fltarr(s[1],s[2],/nozero)
;jcleaned=fltarr(s[1],s[2],/nozero)
;print,jbmethod
;if jbmethod eq 'FFT' then begin
;    drip_message,'FFT jailbar correction not available, using MEDIAN'
;    jbmethod='MEDIAN'
    ;jailbar=data
    ; Generate FFT of image
    ;fft_data = fft(jailbar)
    ; Create Mask in Fourier space that zeros out periods of 16 pixels
    ;indx = findgen(15)*16.0 + 16.0
    ;jbmask = complexarr(256,256) + 1.0
    ;jbmask[indx,*] = 0.0
    ; Apply Mask
    ;jbcleaned = fft(fft_data*jbmask,/INVERSE) ; abs(fft(fft_data*jbmask,/INVERSE))
;endif
if jbmethod eq 'MEDIAN' then begin
    ; Clean "jailbar" pattern noise from background-subtracted image using median
    ; of correlated columns (every 16)
    sm_box = 16 ; smoothing box in pixels
    jailbar=data-filter_image(data, median = sm_box, /all_pixels)  ; Smooth (median)
    index=indgen(256/16)*16  ;index every 16th pixel in a row
    for k=0,255 do begin
        for j=0,15 do begin
            jailbar(index+j,k)=median(jailbar(index+j,k))
        endfor
    endfor
    jbcleaned = data-jailbar
endif

if keyword_set(basehead) then $
      sxaddpar,basehead,'HISTORY','Jail bar correction using median and subtracting smoothed image with 16x16 box'

return, jbcleaned

end
