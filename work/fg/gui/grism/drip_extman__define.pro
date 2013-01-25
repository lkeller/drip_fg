; NAME:
;     DRIP_EXTMAN__DEFINE - Version .7.0
;
; PURPOSE:
;     Analysis Object Manager for the GUI
;
; CALLING SEQUENCE:
;     Obj=Obj_new('DRIP_EXTMAN', BASEID)
;
; INPUTS:
;     BASEID - Widget ID of base to put widgets
;
; STRUCTURE:
;     TITLE - object title
;     FOCUS - focus status (1 if in focus else 0)
;     DISOBJ - display object
;     BASEWID - base widget
;
; HISTORY;
; =============================
; 7/23/10 - Josh Cheng and Casey Deen added nodding extraction mode
; 8/5/10  - Rob Lewis -Ithaca College changed map for most recent cooldown
pro drip_extman::setmap,mode
; mode - grism mode
;         0 - G1xG2
;         1 - G3xG4
;         2 - G1
;         3 - G3
;         4 - G5
;         5 - G6

;  This case statement could eventually come from the header file or master flat.
; 
case mode of
    0:begin            ; G1xG2
        ;   [ [ [y0_bottom_left, y0_bottom_right], [y1_bl, y1_br], ...], [ [x0_left, x0_right], [x1_l, x1_r], ... ] ]
      
        ; *self.map=[[[202,236],[163,210],[128,173],[97,140],[70,110],[45,84],[23,60],[0,37]],$
        ;            [[1,168]  ,[1,254]  ,[1,254]  ,[1,254] ,[1,254] ,[1,254],[1,254],[1,254]]]
        ; *self.ord_height=[19,19,19,19,18,17,16,16]

         *self.map=[[[194,239],[156,202],[124,165],[93,135],[68,106],[42,81],[23,56],[3,36]],$
                    [[0,234]  ,[0,255]  ,[0,255]  ,[0,255] ,[0,255] ,[0,255],[0,255],[0,255]]]

         *self.ord_height=[15,15,15,15,15,15,15,15]

         *self.orders=[15, 16, 17, 18, 19, 20, 21, 22]
         
      end
    1:begin            ; G3xG4
         *self.map=[[[137,231],[80,164],[33,112],[0, 69],[1, 33]], $
         [[0,255],[0,255],[0,255],[24,255],[141,255]]] 
         *self.ord_height = [19, 19, 19, 19,17]
         *self.orders=[7, 8, 9, 10, 11]
      end
    2:begin           ; G1
         *self.map=[[[0,0]],[[0,255]]]
         *self.ord_height = [255]
         *self.orders=[1]
      end
    3:begin           ; G3
         *self.map=[[[0,0]],[[0,255]]]
         *self.ord_height = [255]
         *self.orders=[1]
      end
    4:begin           ; G5
         *self.map=[[[0,0]],[[0,255]]]
         *self.ord_height = [255]
         *self.orders=[1]
      end
    5:begin           ; G6
         *self.map=[[[0,0]],[[0,255]]]
         *self.ord_height = [255]
         *self.orders=[2]
      end
endcase

end

;**********************************
;    LORENTZ - lorentz function
;**********************************
function lorentz, x, x_c, gamma

; gamma = half-width at half-maximum

y = fltarr(n_elements(x))

for i = 0, n_elements(x)-1 DO BEGIN
   y[i] = (gamma/( (x[i]-x_c)^2.0 + gamma^2.0))/3.14159
ENDFOR

return, y

END

;******************************************************************************
;   Pre-defined extraction 
;******************************************************************************
pro drip_extman::predefined_extraction,mode,dapname

common drip_config_info, dripconf

data=*self.data
self->setmap,mode
map=*self.map

; Get information from FITS header
header = self.dataman->getelement(dapname,'HEADER')

caldata = drip_getpar(header,'caldata')
readcol, caldata+'wavecal.txt', grism_mode, orders, Coeff_0, Coeff_1, Coeff_2, Coeff_3, FORMAT='A,I,F,F,F,F', skipline = 1

default_wavecal = 1  ; Use default unless we find a wavcal map

n_orders=(n_elements(*self.orders))   ; number of extractions/orders


;extraction_mode = drip_getpar(header, 'EXTMODE')
extraction_mode = 'FULLAP'
source_type = drip_getpar(header, 'SRCTYPE')
if source_type eq 'POINT_SOURCE' then extraction_mode = 'OPTIMAL' 

instrument_mode = drip_getpar(header, 'INSTMODE')
if (uint(sxpar(header,'C2NC2')) eq 1) then instrument_mode = 'C2NC2'

case mode of
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

avg=0

for i=0,n_orders-1 do begin
    ; calculates the slope of the ROI box
    pos = where( (orders eq (*self.orders)[i]) and (grism_mode eq grmode_txt) )

    slope= float(map[1,i,0]-map[0,i,0])/float(map[1,i,1]-map[0,i,1])
    ;xvalues
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
    
    dy = (*self.ord_height)[i]  ; The height of the order (spatial direction)
                          ; This should be calculated from the order map
                          ; Fixed value for now
    
    ; Isolate a single order
    
    ;Used in defining background(sky) region
    psf_fwhm = 6.0
    pix_throw = 2*psf_fwhm ; eventually calculate pix_throw from drip_getpar(header, 'CHPAMP1')
    
    sub_array = fltarr(n_elements(xvalue),dy+1)
    bkg_sub_array = fltarr(n_elements(xvalue),psf_fwhm+1) ; Sub array for background
    segment_size=floor(n_elements(xvalue)/n_segments)
    yabsice = findgen(n_elements(data[0,*]))
    
    
    for k= 0,n_elements(xvalue)-1 do begin
       shifted_data = interpol(data[xvalue[k],*],yabsice,yabsice+yvalue[k])
       sub_array[k,*] = shifted_data[0:dy]
    endfor
    
    ;for k= 0,n_elements(xvalue)-1 do begin
       
       ;shifted_data1 = interpol(data[xvalue[k],*],yabsice,yabsice+yvalue[k]+psf_fwhm+pix_throw)
       ;shifted_data2 = interpol(data[xvalue[k],*],yabsice,yabsice+yvalue[k]-psf_fwhm-pix_throw)
       ;bkg_sub_array[k,*] = (shifted_data1[0:psf_fwhm]+shifted_data2[0:psf_fwhm])/2
       bkg_sub_array = (shift(sub_array,0,pix_throw)+ $
                        shift(sub_array,0,-pix_throw))/2 ; mean of bkg on either side of spec trace
       
       ;atv22,bkg_sub_array                                      
    ;endfor
  
    case extraction_mode of
        'OPTIMAL': begin
        drip_message,'Using OPTIMAL Extraction method'
        
          ; Optimal Extraction Begins here
          ; "Optimal" here means psf-weighted, but the psf is
          ; estimated from a Lorentzian fit to the spatial profile
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
                    xx[k] = (k+0.5)*segment_size  ; Positions segment centoirds
                    yy[k] = prev_order[k]
                    
                 ENDELSE
             endfor
             
             good_fits = where(fit_status ne 5)
             
             ;Fit a polynomial to the spectral trace centroids
             fit_result = POLY_FIT(xx[good_fits],yy[good_fits],2)
             
             ;Fit results go here
             x = findgen(n_elements(sub_array[*,0]))
             y = fit_result[0] + fit_result[1]*x + fit_result[2]*x^2

             ycoord = findgen(n_elements(sub_array[0,*]))
              
             for k = 0, n_elements(sub_array[*,0])-1 DO BEGIN
                
                j = floor(k/(segment_size+1)) ; Index for Gaussian HWHM 

                filter = gaussian(ycoord,[10.0,y[k],sigma[j]]) ;Weighting function for extraction
                
                extracted_spectrum[k] += total((sub_array[k,*])* $
                                               filter/max(filter), /NAN) -$
                total((bkg_sub_array[k,*])* $
                                               filter/max(filter), /NAN)
                                                           
             ENDFOR
            
          ENDFOR
         
       END
        'FULLAP' : begin
          drip_message,'drip_anal_extract: Using FULL Aperture Extraction method'
      
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

          ;for k = 0, n_elements(extracted_spectrum)-1 DO BEGIN
          ;   extracted_spectrum[k] = total((sub_array[k,*]), /NAN)
          ;ENDFOR
       end
    ENDCASE

    ;extracted_spectrum = extracted_spectrum - extracted_bkg

    if (i eq 0) then avg1=mean(extracted_spectrum) ; Roughly averages spectra to be on the same scale...
    
    avg=mean(extracted_spectrum)
    if (i eq 0) then begin
        davg=avg-avg1
    endif else davg=avg
    extracted_spectrum=extracted_spectrum-davg

    if default_wavecal eq 0 then *self.allwave[i] = lmap[*,0]
    if default_wavecal eq 1 then *self.allwave[i]=wave
    *self.allflux[i]=extracted_spectrum
 endfor

; G3, G5, and G6 have lambda increaseing right to left (all others are left
; to right. So flitp G5 and G6 'allflux' arrays.

;if (mode eq 3) OR (mode eq 4) OR (mode eq 5) then begin
;   print,'Reversing g5 or g6 wavelength order'
;   self.allflux=reverse(self.allflux) ; reverse order so lambda left --> right
   ;allerror=reverse(allerror) 
   ;extracted =[[allwave],[allflux],[allerror],[ext_orders]]
;endif else begin
   ;extracted = [[allwave],[allflux],[allerror],[ext_orders]]
;endelse

drip_message,'drip_extman: Pre-defined Extraction FINISHED'

end
;******************************************************************************
;     Get Data - Send new sets of data
;******************************************************************************
function drip_extman::getdata,data=data,extract=ext,dapsel_name=dapn,$
                    wave_num=wave_num, flux_num=flux_num,$
                    orders=orders
if keyword_set(data) then  return, self.data
if keyword_set(ext) then return, self.extract
if keyword_set(dapn) then return, self.dapsel_name
if keyword_set(wave_num) then return, *self.allwave[abs((*self.orders)[0]-wave_num)]
if keyword_set(flux_num) then return, *self.allflux[abs((*self.orders)[0]-flux_num)]
if keyword_set(orders) then return, (*self.orders)

end
;******************************************************************************
;     Covert Coordinates
;******************************************************************************

pro drip_extman::convcoord
self.boxx0= self.boxu0
self.boxy0= self.boxv0
self.boxx1= self.boxu1
self.boxy1= self.boxv1
end

;******************************************************************************
;     Extract - Extract from the data
;   This routine is used solely for extraction of user-defined ROI
;******************************************************************************
pro drip_extman::user_defined_extraction

print,'extman::user_defined_extraction (*****)'
common drip_config_info, dripconf

data=*self.data

dy=self.boxy1-self.boxy2 ; height

slope= float(self.boxy2-self.boxy0)/float(self.boxx2-self.boxx0) ;slope

header = self.dataman->getelement(self.dapsel_name,'HEADER')
extraction_mode = 'FULLAP'  ;drip_getpar(header, 'EXTMODE')
instrument_mode = drip_getpar(header, 'INSTMODE')

;Get wavecal data from wavecal.txt
datadir = drip_getpar(header, 'CALDATA')
if datadir eq 'x' then begin
  print,'drip_extman::user_defined_extraction -Error finding wavecal.txt'
  print,'drip_extman::user_defined_extraction - Spectrum extraction failed'
  return
endif
;Get wavecal data from wavecal.txt
readcol, datadir+'wavecal.txt', grism_mode, orders, Coeff_0, Coeff_1, Coeff_2, Coeff_3, FORMAT='A,I,F,F,F,F', skipline = 1

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

;determine xvalues for ROI
xvalue=round(findgen(self.boxx2-self.boxx0+1) + self.boxx0)

;mx=float((lam_high(pos)-lam_low(pos)))/float(map[1,i,1]-map[0,i,1])     ; Wavelength cal?
;wave=mx(0)*xvalue + (lam_low(pos))(0)
;C0 = coeff_0[pos]
;C1 = coeff_1[pos]
;C2 = coeff_2[pos]
;C3 = coeff_3[pos]
;print,c0,c1,c2,c3

;wave = C0 + C1*xvalue + C2*(xvalue)^2.0 + C3*(xvalue)^3.0
; NEED TO USE 'WAVE' NOW INSTEAD OF 'XVALUE' TO ADD WAVELENGTH CALIBRATION HERE
; 
;determine yvalues for ROI

yvalue=round(slope*(xvalue-self.boxx0))+self.boxy0  

sub_array = fltarr(n_elements(xvalue),dy+1)  ; ROI for extraction

for i= 0,n_elements(xvalue)-1 do begin
    sub_array[i,*]=data[xvalue[i],yvalue[i]:(yvalue[i]+dy)]
endfor

case extraction_mode of
   'OPTIMAL': begin
              ; Optimal Extraction Begins here
              
                  
          drip_message,'Using OPTIMAL Extraction method'
       
          ; Optimal Extraction Begins here
          ; "Optimal" here means psf-weighted, but the psf is
          ; estimated from a Lorentzian fit to the spatial profile
          extracted_spectrum = fltarr(n_elements(sub_array[*,0]))
          segment_size=floor(n_elements(xvalue)/n_segments)
          
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
       drip_message,'Using FULL Aperture Extraction method'
      
          ; Full Aperture Extraction
          extracted_spectrum = fltarr(n_elements(sub_array[*,0]))
          for k = 0, n_elements(extracted_spectrum)-1 DO BEGIN
             extracted_spectrum[k] = total(sub_array[k,*], /NAN)
          ENDFOR
       end
    endcase
  
 if (i eq 0) then avg1=mean(extracted_spectrum) ; Roughly averages spectra to be on the same scale...
    
 avg=mean(extracted_spectrum)
 if (i eq 0) then begin
     davg=avg-avg1
 endif else davg=avg
 extracted_spectrum=extracted_spectrum-davg

; G5 and G6 have lambda increaseing right to left (all others are left
; to right. So flitp G5 and G6 'allflux' arrays.

;if (gmode eq 4) OR (gmode eq 5) then begin
;   print,'Reversing g5 or g6 wavelength order'
;   allflux=reverse(allflux) ; reverse order so lambda left --> right
;   allerror=reverse(allerror) 
;   *self.extracted =[[allwave],[allflux],[allerror]]
;endif else begin  
   
*self.extract = extracted_spectrum         

end


;******************************************************************************
;     NEWDATA - Sets new sets of data
;******************************************************************************

pro drip_extman::newdata,data=data,$
                         boxx0=boxx0, boxy0=boxy0,$
                         boxx1=boxx1, boxy1=boxy1,$
                         boxx2=boxx2, boxy2=boxy2,map=map,$
                         dapsel_name=dapsel_name


;print,'SETTING DATA IN EXTMAN'
if keyword_set(data) then self.data=data
if keyword_set(boxx0) then self.boxx0=boxx0 else self.boxx0=0
if keyword_set(boxy0) then self.boxy0=boxy0 else self.boxy0=0
if keyword_set(boxx1) then self.boxx1= boxx1
if keyword_set(boxy1) then self.boxy1= boxy1
if keyword_set(boxx2) then self.boxx2=boxx2 else self.boxx2=0
if keyword_set(boxy2) then self.boxy2=boxy2 else self.boxy2=0
if keyword_set(map) then *self.map=map
if keyword_set(dapsel_name) then self.dapsel_name=dapsel_name

end
;******************************************************************************
;     SETDATA - Adjust the SELF structure elements
;******************************************************************************

pro drip_extman::setdata, disp_channels=disp_channels
if keyword_set(disp_channels) then self.disp_channels= disp_channels
end


;******************************************************************************
;     RESET - Reset the object
;******************************************************************************
pro drip_extman::reset

;free memory


end



;******************************************************************************
;      CLEANUP
;******************************************************************************

pro drip_extman::cleanup
;free pointers
ptr_free,self.extract
ptr_free,self.fileinfoval
ptr_free,self.orders
ptr_free,self.ord_height
ptr_free,self.map
end


;******************************************************************************
;      INIT
;******************************************************************************

function drip_extman::init,mw, dataman

self.mw = mw
self.dataman=dataman
;memory for fileinfo values
self.fileinfoval=ptr_new(/allocate_heap)
self.dapsel=ptr_new(/allocate_heap)
self.extract=ptr_new(/allocate_heap)
self.allflux=ptrarr(100,/allocate_heap)
self.allwave=ptrarr(100,/allocate_heap)
self.map=ptr_new(/allocate_heap)
self.orders=ptr_new(/allocate_heap)
self.ord_height=ptr_new(/allocate_heap)
self.boxx0=0
self.boxy0=0
self.boxx1=0
self.boxy1=0
self.boxy2=0
self.boxx2=0

return,1
end


;******************************************************************************
;      DRIP_EXTMAN__DEFINE
;******************************************************************************
pro drip_extman__define

struct={drip_extman,$
        dataman:obj_new(), $    ;data manager
        boxx0:0,boxy0:0,$       ;lower left corner (data coordinates)
        boxx1:0,boxy1:0,$       ;top right corner (data coordinates)
        boxx2:0, boxy2:0,$      ;lower right corner
        disp_channels:objarr(4),$ ;display channels
        dapsel_name:'',$        ;name of currently selected dap
        dapsel:ptr_new(),$      ;selected dap
        data:ptr_new(),$        ;from currently selected dap
        extract:ptr_new(),$     ;extracted data
        mw:obj_new(),$          ;wid of file information table
        fileinfo:0L,$           ;wid of file information table
        fileinfoval:ptr_new(),$ ;file information table
        allflux:ptrarr(100),$   ;all flux data
        allwave:ptrarr(100),$   ;all wave data
        map:ptr_new(),$         ;list of y-coordinates for extraction
        orders:ptr_new(),$      ;[array with order numbers]
        ord_height:ptr_new(),$  ;[array of order heights (in pixels)]
        n:0}   ;,$
        ;inherits drip}          ; child object of drip}
end
