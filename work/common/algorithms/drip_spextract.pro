; NAME:
; DRIP_SPEXTRACT - Version 1.0
;
; PURPOSE:
; Pipeline spectral extraction for all FORCAST grism spectral modes
;
; CALLING SEQUENCE:
;       SPEXTRACTED=DRIP_SPEXTRACT(DATA, HEADER)
;
; INPUTS:
;       DATA - the reduced spectral image to be extracted
;       HEADER - The fits header of the new input data file
;             
;
; SIDE EFFECTS: None identified
; 
;
; RESTRICTIONS: None
; 
;
; PROCEDURE:
; Read reduced data intro extraction routines, extract, plot 1-D
; spectrum. Uses existing extraction code in drip_extman.
;
; MODIFICATION HISTORY:
;   Written by:  Luke Keller, Ithaca College, September 29, 2010
;   Modified     Luke Keller, Rob Lewis, Ithaca College, July 7, 2011
;                Moved extraction code from drip_extman__define.pro
;                now it will be a pipline step rather than gui object.
;   Modified     Luke Keller, Rob Lewis, Ithaca College, August 2, 2011
;                Changed names of grisms to apply to most recent
;                forcast setup
;   Modified     Rob Lewis, Ithaca College, August 5,2011 spectral map/masks
;                updated for new filter wheel/grism configurations
;   Modified     Michael Pavel (UT Austin) and Luke Keller, Ithaca College
;                Full-aperture extraction now looks for and used a
;                wavecal map in the calibration data area. Uses
;                default polynomical coeffs in wavcal.txt if no maps
;                found. Also now rectifying curvature in spectral lines.

;******************************************************************************
; DRIP_SPEXTRACT - Pipeline spectral extraction
;******************************************************************************

function drip_spextract, data, header, gmode

common drip_config_info, dripconf

map=map

;Read and prepare the default wavelength calibration (in case the
;individual wavecal maps (FITS files) for a modes can't be found.
caldata=drip_getpar(header,'caldata')

readcol, caldata+'wavecal.txt', grism_mode, orders, Coeff_0, Coeff_1, Coeff_2, $
    Coeff_3, FORMAT='A,I,F,F,F,F', skipline = 1

; Wavecal uses wavecal image (more precise than the default above) if
; the wavecal map is found

drip_message, 'Default wavecal file is: '+caldata+'wavecal.txt'
default_wavecal = 1  ; Use default unless we find a wavcal map

if (gmode gt 1) then n_orders = 1
if (gmode eq 1) then n_orders = 5
if (gmode eq 0) then n_orders = 8

case gmode of
   0: begin
      grmode_txt = 'G1xG2'
      lmap_file = 'g1xg2_lmap.fits'
      end
   1: begin
      grmode_txt = 'G3xG4'
      lmap_file = 'g3xg4_lmap.fits'
      end
   2: begin
      grmode_txt = 'G1'
      lmap_file = 'g1_lmap.fits' 
      end
   3: begin
      grmode_txt = 'G3'
      lmap_file = 'g3_lmap.fits'
      end
   4: begin
      grmode_txt = 'G5'
      lmap_file = 'g5_lmap.fits'
      end
   5: begin
      grmode_txt = 'G6'
      lmap_file = 'g6_lmap.fits'
      end
endcase

lmap=readfits(caldata+lmap_file,/noscale,/silent)
if size(lmap,/N_dimensions) gt 0 then begin
    ; Use lmap for wavecal;
    default_wavecal = 0
endif  else begin
    ; no valid lmap data loaded - use default wavecal
    drip_message, 'spextract:  Error Loading wavelength map, using default wavecal'
endelse


;if gmode eq 2 then begin
;    lmap_file = 'DEMO_GRISM_DATA_v2.3/Cal/g1_lmap.fits'
;    lmap = readfits(lmap_file)
;endif

orderslist=orders

;extraction_mode = drip_getpar(header, 'EXTMODE')
instrument_mode = drip_getpar(header, 'INSTMODE')
;print,'C2NC2= ', (uint(drip_getpar(header,'C2NC2')) eq 1)
if (uint(sxpar(header,'C2NC2')) eq 1) then instrument_mode = 'C2NC2'
;instrument_mode = 'C2NC2'
extraction_mode = 'FULLAP'
source_type = drip_getpar(header, 'SRCTYPE')
if source_type eq 'POINT_SOURCE' then extraction_mode = 'OPTIMAL' 

; NEED tapered column extraction option    

;  This information should actually come from the header file or dripconf.txt
; 
case gmode of
    0:begin            ; G1xG2
         ;map=[[[202,236],[163,210],[128,173],[97,140],[70,110],[45,84],[23,60],[0,37]],$
         ;     [[1,168]  ,[1,254]  ,[1,254]  ,[1,254] ,[1,254] ,[1,254],[1,254],[1,254]]]

         map=[[[194,239],[156,202],[124,165],[93,135],[68,106],[42,81],[23,56],[3,36]],$
              [[0,234]  ,[0,255]  ,[0,255]  ,[0,255] ,[0,255] ,[0,255],[0,255],[0,255]]]

         ord_height=[15,15,15,15,15,15,15,15]
         orders=[15, 16, 17, 18, 19, 20, 21, 22]
      end
    1:begin            ; G3xG4
         map=[[[137,231],[80,164],[33,112],[0, 69],[1, 33]], $
         [[0,255],[0,255],[0,255],[24,255],[141,255]]] 
         ord_height = [19, 19, 19, 19, 17]
         orders=[7, 8, 9, 10, 11]
      end
    2:begin           ; G1
         map=[[[0,0]],[[0,255]]]
         ord_height = [255]
         orders=[1]
         ;data=rot(data,90.0)
      end
    3:begin           ; G3
         map=[[[0,0]],[[0,255]]]
         ord_height = [255]
         orders=[1]
         ;data=rot(data,90.0)
      end
    4:begin           ; G5
         map=[[[0,0]],[[0,255]]]
         ord_height = [255]
         orders=[1]
         ;data=rot(data,90.0)
      end
    5:begin           ; G6
         map=[[[0,0]],[[0,255]]]
         ord_height = [255]
         orders=[2]
         ;data=rot(data,90.0)
         
      end
endcase

case instrument_mode of
    'C2N': begin
         if drip_getpar(header, 'CNMODE') eq 'NMC' then begin
             c = [1]  ; Extract a single positive spectrum
             n_segments = 16 ; Extract spectrum in 'n_segments' sections
         endif else begin
             c = [1, -1] ; Extract both positive and negative spectra
             n_segments = 16
         endelse
    END
    'C2NC2': begin
         c = [1]
         n_segments = 16
    END
    'STARE': begin
         c = [1]
         n_segments = 16
    END
    'NOS': begin
         c = [1]
         n_segments = 16
    END
    'NAS': begin
         c = [1, -1]
         n_segments = 16
    END
endcase

allwave=fltarr(1)
allflux=fltarr(1)
avg=0
ext_orders=fltarr(1)
prev_order = fltarr(n_segments)
for i=0,n_orders-1 do begin
    ; Determine the sub array that contains the spectral order
    ; 

    pos = where( (orderslist eq orders[i]) and (grmode_txt eq grism_mode) )
  
    ; Calculate the slope of the spectral trace from the order map.
    ; Used as a first guess when fitting the spectral trace.
    
    slope= float(map[1,i,0]-map[0,i,0])/float(map[1,i,1]-map[0,i,1])
    
    ;xvalues, the number of pixels (dispersion direction) in this order
    
    xvalue=findgen(map[1,i,1]-map[0,i,1]+1)
    
    ; Apply predetermined wavelenth calibration from wavecal.txt data
    ; These data should eventually go into the dripconf.txt file
    
    ; Wavecal polynomial coefficients (lambda in microns vs. pixels)
    C0 = coeff_0[pos]
    C1 = coeff_1[pos]
    C2 = coeff_2[pos]
    C3 = coeff_3[pos]
    wave = C0[0] + C1[0]*xvalue + C2[0]*(xvalue)^2.0 + C3[0]*(xvalue)^3.0

    sxaddpar, header, 'HISTORY', 'Wavecal coeffs:'+string(c0)+string(c1)+string(c2)+string(c3)

    ;yvalue, the spatial direction positions of the order in the array,
    ; calculated from the order slope
    
    yvalue=slope*xvalue+map(0,i,0)
    
    ; Build a sub array for extracted data
    
    dy = (ord_height)[i]  ; The height of the order (spatial direction)
                          ; calculated from the order map
                          
    
    ; Isolate a single order

    ;Used in defining background(sky) region
    psf_fwhm = 6.0
    pix_throw = 10*psf_fwhm ; eventually calculate pix_throw from drip_getpar(header, 'CHPAMP1')
    
    sub_array = fltarr(n_elements(xvalue),dy+1)
    bkg_sub_array = fltarr(n_elements(xvalue),2*psf_fwhm+1) ; Sub array for background
    segment_size=floor(n_elements(xvalue)/n_segments)
    yabsice = findgen(n_elements(data[0,*]))
    
    
    for k= 0,n_elements(xvalue)-1 do begin
       shifted_data = interpol(data[xvalue[k],*],yabsice,yabsice+yvalue[k])
       sub_array[k,*] = shifted_data[0:dy]
    endfor

    ;Extract background from 2*PSF_FWHM outside of the chop/nod throw
    
    ;for k= 0,n_elements(xvalue)-1 do begin
       
       ;shifted_data1 = interpol(data[xvalue[k],*],yabsice,yabsice+yvalue[k]+pix_throw)
       ;shifted_data2 = interpol(data[xvalue[k],*],yabsice,yabsice+yvalue[k]-pix_throw)
       ;bkg_sub_array[k,*] = (shifted_data1[0:2*psf_fwhm]+shifted_data2[0:2*psf_fwhm])/2 
       bkg_sub_array = (shift(sub_array,0,pix_throw)+ $
                        shift(sub_array,0,-pix_throw))/2 ; mean of bkg on either side of spec trace
       
       ;print,pix_throw,'HERE in spectract'
    ;endfor
    
    case extraction_mode of
       'OPTIMAL': begin
       drip_message,'Using OPTIMAL Extraction method'
       sxaddpar,header,'HISTORY','Background subtraction during extraction: ON'
          ; Optimal Extraction Begins here
          ; "Optimal" here means psf-weighted, but the psf is
          ; estimated from a Gaussian fit to the spatial profile
          extracted_spectrum = fltarr(n_elements(sub_array[*,0]))
          extracted_bkg = fltarr(n_elements(sub_array[*,0]))
          
          ;For each chopped spectral image
          for j = 0, n_elements(c)-1 DO BEGIN
             
             sub_array *= c[j] ; For each chop/nod-subtracted spectrum
             
             xx = intarr(n_segments)  ; Place for trace fit data
             yy = fltarr(n_segments)
             
             fit_status = intarr(n_segments)
             sigma = fltarr(n_segments) ; Gaussian fit stdev for spatial profile (PSF)
             
             ; For each segment of the spectral trace
             for k = 0,n_segments-1 do begin  
                segment = sub_array[k*segment_size:(k+1)*segment_size-1,*]
                
                ;Estimate the spatial profile of this segment
       
                ;Median subtract to make sure background level is around zero
                ;Median filter to supress jailbars
                profile = median(total((segment-median(segment)),1, /NAN),3) 
                
                ;Fit this (spatial) profile with a Gaussian distribution
                positive = where(profile ge 0)  ; Don't try to fit negative values
                if (positive[0] ne -1) THEN BEGIN
                    profile_coord = findgen(n_elements(profile)) ; Fit data points go here
                   
                    profile_fit = mpfitpeak(profile_coord[positive],$
                       profile[positive], a, NTERMS=3, STATUS=status, /gaussian)
                    xx[k] = (k+0.5)*segment_size
                    yy[k] = a[1] ; Centroid of the fitted Gaussian
                    
                    sigma[k] = a[2] ; Gaussian stdev for each segment

                    fit_status[k] = status
                 ENDIF ELSE BEGIN
                    xx[k] = (k+0.5)*segment_size  ; Positions segment centroids
                    yy[k] = prev_order[k]
                    
                 ENDELSE
             endfor
             
             good_fits = where(fit_status ne 5)
             
             ;Fit a polynomial to the spectral trance centroids
             fit_result = POLY_FIT(xx[good_fits],yy[good_fits],2)
             
             ;Fit results go here
             x = findgen(n_elements(sub_array[*,0]))
             y = fit_result[0] + fit_result[1]*x + fit_result[2]*x^2

             ycoord = findgen(n_elements(sub_array[0,*]))
              
             for k = 0, n_elements(sub_array[*,0])-1 DO BEGIN
                
                j = floor(k/(segment_size+1)) ; Index for Gaussian HWHM 

                filter = gaussian(ycoord,[10.0,y[k],sigma[j]]) ;Weighting function for extraction
                
                extracted_spectrum[k] += total((sub_array[k,*])* $
                                               filter/max(filter), /NAN) - $

                                         total((bkg_sub_array[k,*])* $
                                               filter/max(filter), /NAN)
                                                           
             ENDFOR
            
          ENDFOR
         
       END
       'FULLAP' : begin
       drip_message,'Using FULL Aperture Extraction method'
      
       ; Full Aperture Extraction

       extracted_spectrum = fltarr(n_elements(sub_array[*,0]))
       
       ; Use the wavecal image if default is 0 (NO)
       if default_wavecal eq 0 then begin
           ;all rows will be wavelength calibrated with respect to first row
           ;Calculate wavelength shift of each row wrt first row
           wl_shift = shift(lmap[0,*],-1)-lmap[0,0]


           ;calculate average pixel resolution
           rmap = shift(lmap,-1,0)-lmap
           resolution = mean(rmap[0:254,*]);microns/pixel


           pix_shift = (wl_shift/resolution)

           ;oversample data by a factor of x, shift wavelengths, and reform pixels
           x = 10
           pix_shift_over = round(pix_shift*x)
           nrow = 256
           ncol = 256

           out_image = fltarr(ncol,nrow)
           for ii=0,nrow-1 do begin; for each row
               vec = data[*,ii]
               vec_over = fltarr(x*n_elements(vec));create oversampled array
               for kk=0,ncol-1 do begin
                   vec_over[x*kk:x*(kk+1)-1] = vec[kk]/x; conserve flux!
               endfor
               vec_shifted = shift(vec_over,-1*pix_shift_over[255-ii])
               ;reform oversampled spectrum
               vec_out = fltarr(n_elements(vec))
               for kk=0,ncol-1 do begin
                   vec_out[kk] = total(vec_shifted[x*kk:x*(kk+1)-1]); flux still conserved
               endfor
               out_image[*,ii] = vec_out
           endfor
            
           ;sum spectrum in spatial direction
           extracted_spectrum = total(out_image,2)
        endif else begin
           ;No info on row-by-row extraction so just total the array in the spatial direction
           for k = 0, n_elements(extracted_spectrum)-1 DO BEGIN
               extracted_spectrum[k] = total((sub_array[k,*]), /NAN)
           endfor
        endelse
    end
    endcase

    if (i eq 0) then avg1=mean(extracted_spectrum) ; Roughly averages spectra to be on the same scale...
    avg=mean(extracted_spectrum)
    if (i eq 0) then begin
        davg=avg-avg1
    endif else davg=avg
    extracted_spectrum=extracted_spectrum-davg

    if default_wavecal eq 0 then allwave=[allwave,lmap[*,0]]
    if default_wavecal eq 1 then allwave=[allwave,wave]
    allflux=[allflux,extracted_spectrum]

    sz_wave=size(wave)

    list_orders=replicate(orders[i],sz_wave[1],1)
 
    ext_orders=[ext_orders, list_orders]
 endfor

allwave=allwave[1:*,*]
allflux=allflux[1:*,*]
allerror=fltarr(n_elements(allflux))
ext_orders=ext_orders[1:*,*]

; G5 and G6 have lambda increaseing right to left (all others are left
; to right. So flitp G5 and G6 'allflux' arrays.

if (gmode eq 3) OR (gmode eq 4) OR (gmode eq 5) then begin
;   print,'Reversing g5 or g6 wavelength order'
   allflux=reverse(allflux) ; reverse order so lambda left --> right
   allerror=reverse(allerror) 
   extracted =transpose([[allwave],[allflux],[allerror],[ext_orders]])
endif else begin
   extracted = transpose([[allwave],[allflux],[allerror],[ext_orders]])
endelse

sxaddpar,header,'HISTORY','Spectral extraction complete using '+extraction_mode

drip_message,'PIPELINE Extraction FINISHED in drip_spextract'
return, extracted

end
