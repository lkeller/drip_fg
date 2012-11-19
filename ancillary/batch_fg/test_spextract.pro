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

;******************************************************************************
; DRIP_SPEXTRACT - Pipeline spectral extraction
;******************************************************************************

function test_spextract, data, gmode

;common drip_config_info, dripconf

;data=*self.data
;self->setmap,mode

;map=map


;readcol,'drip_gui/order_calb.txt',orders,lam_low,lam_high,format='i,f,f'  ; need to modify to include polynomial fits
;readcol, 'drip_gui/order_calb.txt', grism_mode, orders, lam_low, lam_high, FORMAT='A,I,F,F', comment = '#', delimiter=','
readcol, 'DEMO_GRISM_DATA_v2.0/Cal/wavecal.txt', grism_mode, orders, Coeff_0, Coeff_1, Coeff_2, Coeff_3, FORMAT='A,I,F,F,F,F', skipline = 1
;readcol, 'drip_gui/order_calb.txt', grism_mode, orders, lam_low, lam_high, FORMAT='A,I,F,F', skipline = 1

;wvg1o1=['G1',      1,   4.847, 0.011535156, 0.0, 0.0]
;wvg3o1=['G3',      1,   8.400, 0.0140625, 0.0, 0.0]
;wvg5o1=['G5',      1,   17.500, 0.040234375, 0.0, 0.0]
;wvg6o2=['G6',      2,   28.300, 0.032421875, 0.0, 0.0]
;wvg12o22=['G1xG2',   22,  4.847, 0.00150, 0.0, 0.0]
;wvg12o21=['G1xG2',   21,  5.117, 0.00160, 0.0, 0.0]
;wvg12o20=['G1xG2',   20,  5.3971881, 0.0017011771, 2.9514793e-7, -6.0232318e-10]
;wvg12o19=['G1xG2',   19,  5.6801786, 0.0018058653, 1.2692539e-7, -3.088827e-10]
;wvg12o18=['G1xG2',   18,  5.9924521, 0.0019016197, 3.4609127e-7, -1.0056626e-10]
;wvg12o17=['G1xG2',   17,  6.3437603, 0.0019817931, 4.8657155e-7, -1.0984662e-9]
;wvg12o16=['G1xG2',   16,  6.72309, 0.0025138625, -2.965611e-6, 8.1018120e-9]
;wvg12o15=['G1xG2',   15,  7.1806135, 0.0031013773, 0.0, 0.0]
;wvg34o6=['G3xG4',   6,   12.7,    0.00600, 0.0, 0.0]
;wvg34o7=['G3xG4',   7,   11.400,  0.00546875, 0.0, 0.0]
;wvg34o8=['G3xG4',   8,   10.300,  0.005078125, 0.0, 0.0]
;wvg34o9=['G3xG4',   9,   9.400,   0.0046875, 0.0, 0.0]
;wvg34o10=['G3xG4',   10,  8.700,   0.003515625, 0.0, 0.0]
;wvg34o11=['G3xG4',   11,  8.00,    0.00390625, 0.0, 0.0]

;wav1 = drip_getpar(header,'wvg1o1')

orderslist=orders

;n_orders=orders                ; number of extractions/orders
;print,' n_orders', n_orders
; Gets information from the header
; print, dapname
;header = self.dataman->getelement(dapname,'HEADER')


extraction_mode = 'OPTIMAL' ;drip_getpar(header, 'EXTMODE')
instrument_mode = 'STARE'; drip_getpar(header, 'INSTMODE')
cnmode='NMC'

; print, self.dapsel_name
; print, extraction_mode, instrument_mode

if (gmode gt 1) then n_orders = 1
if (gmode eq 1) then n_orders = 5
if (gmode eq 0) then n_orders = 8

case gmode of
   0: grmode_txt = 'G1xG2'
   1: grmode_txt = 'G3xG4'
   2: grmode_txt = 'G1'
   3: grmode_txt = 'G3'
   4: grmode_txt = 'G5'
   5: grmode_txt = 'G6'
endcase

;  Info in this case statement should actually come from the header file or master flat
;  or dripconf.txt file

; Map of the orders on the array: [xrange, yrange, order]
case gmode of
    0:begin            ; G1xG2
         ;map=[[[202,236],[163,210],[128,173],[97,140],[70,110],[45,84],[23,60],[0,37]],$
         ;           [[1,168]  ,[1,254]  ,[1,254]  ,[1,254] ,[1,254] ,[1,254],[1,254],[1,254]]]
         map=[[[202,236],[163,210],[128,173],[97,140],[70,110],[45,84],[23,60],[0,37]],$
                    [[0,168]  ,[0,255]  ,[0,255]  ,[0,255] ,[0,255] ,[0,255],[0,255],[0,255]]]
         ;ord_height=[19,19,19,19,18,17,16,16]
         ord_height=[19,19,19,19,19,19,19,19]
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
         if cnmode eq 'NMC' then begin
             c = [1]  ; Extract a single positive spectrum
             n_segments = 16 ; Extract spectrum in 'n_segments' sections
         endif else begin
             c = [1, -1] ; Extract both positive and negative spectra
             n_segments = 16 ; Divide order into n_segments for extraction
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
    
    ;yvalue, the spatial direction positions of the order in the array,
    ; calculated from the order slope
    
    yvalue=slope*xvalue+map(0,i,0)
    
    ; Build a sub array for extracted data
    
    dy = (ord_height)[i]  ; The height of the order (spatial direction)
                          ; This should be calculated from the order map
                          ; Fixed value for now
     

    sub_array = fltarr(n_elements(xvalue),dy+1)
    
    ;bkg_sub_array = fltarr(n_elements(xvalue),dy+1) ; Sub array for background
    
    ; Isolate a single order
    
    sub_array = fltarr(n_elements(xvalue),dy+1)
    segment_size=floor(n_elements(xvalue)/n_segments)
    yabsice = findgen(n_elements(data[0,*]))
    
    
    for k= 0,n_elements(xvalue)-1 do begin
       shifted_data = interpol(data[xvalue[k],*],yabsice,yabsice+yvalue[k])
       sub_array[k,*] = shifted_data[0:dy]
    endfor

    for k= 0,n_elements(xvalue)-1 do begin
       
    ;THIS IS WHERE WE'LL GET THE BACKGROUND SUBTRACTION SUBARRAY 
       
    endfor
 
    atv22,sub_array
   
    
    case extraction_mode of
       'OPTIMAL': begin
       print,'Using OPTIMAL Extraction method'
       
          ; Optimal Extraction Begins here
          ; "Optimal" here means psf-weighted, but the psf is
          ; estimated from a Lorentzian fit to the spatial profile
          extracted_spectrum = fltarr(n_elements(sub_array[*,0]))
          
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
                
                ;Fit this (spatial) profile with a Lorentzian distribution
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
                    xx[k] = (k+0.5)*segment_size  ; Positions segment centoirds
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
                
                extracted_spectrum[k] += total(sub_array[k,*]* $
                                               filter/max(filter), /NAN)
                                                           
             ENDFOR
            
          ENDFOR
         
       END
       'FULLAP' : begin
       print,'Using FULL Aperture Extraction method'
      
          ; Full Aperture Extraction
          extracted_spectrum = fltarr(n_elements(sub_array[*,0]))
          for k = 0, n_elements(extracted_spectrum)-1 DO BEGIN
             extracted_spectrum[k] = total(sub_array[k,*], /NAN)
          ENDFOR
       end
    endcase

    if (i eq 0) then avg1=mean(extracted_spectrum) ; Roughly averages spectra to be on the same scale...
    avg=mean(extracted_spectrum)
    davg=avg-avg1
    extracted_spectrum=extracted_spectrum-davg
    
    allwave=[allwave,xvalue] ;allwave=[allwave,wave]
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

if (gmode eq 4) OR (gmode eq 5) then begin
;   print,'Reversing g5 or g6 wavelength order'
   allflux=reverse(allflux) ; reverse order so lambda left --> right
   allerror=reverse(allerror) 
   extracted =[[allwave],[allflux],[allerror],[ext_orders]]
endif else begin
   extracted = [[allwave],[allflux],[allerror],[ext_orders]]
endelse

return, extracted

end
