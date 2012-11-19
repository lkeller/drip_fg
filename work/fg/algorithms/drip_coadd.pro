; NAME:
;     DRIP_COADD - Version .7.0
;
; PURPOSE:
;     Coadd new sequence to existing data
;
; CALLING SEQUENCE:
;     COADDED=DRIP_COADD(NEWDATA, COADDED, HEADER, BASEHEAD, /FIRST)
;
; INPUTS:
;     NEWDATA - The new reduced image
;     COADDED - Coadded previous images
;     HEADER - The fits header of the new input data file
;     BASEHEAD - The fits header of the first data file
;     /FIRST - Flag for first coadding
;
; OUTPUTS:
;     COADDED - New Coadded Image
;
; SIDE EFFECTS:
;     None.
;
; RESTRICTIONS:
;     None.
;
; PROCEDURE:
;     Sum new data over old data
;
; MODIFICATION HISTORY:
;   Written by:   Marc Berthoud, CU, July 2004
;   Modified:     Marc Berthoud, CU, November 2004
;                 Added TPCD and C3PD, changed case to switch
;   Modified:     Marc Berthoud, CU, March 2005
;                 Added TEST mode
;                 Renamed all modes
;   Modified:     Marc Berthoud, Palomar, September 2007
;                 Added use of RA and DEC for noding and dithering
;   Modified:     Luke Keller, Ithaca College June 2010
;                 Added option to coadd using cross correlation of
;                 frames
;   Modifed:      Marc Berthoud, Ithaca College, July 2010
;                 Added use of border and resize variables to sample
;                 the final result back down to 256x256 pixels
;   Modifed:      Luke Kelelr, Ithaca College, May 2011
;                 Added dithering to C2 and C2N modes since they are
;                 used in flights in place of C2ND and C2D.
;                 Added rotation of field in coadd step using SKY_ANGL
;                 data from FITS headers.
;                 Fixed S/N calculation in POINT object so it works 
;                 on pixel data rather than e-/s.
;                 Added test for NOD-MATCH-CHOP mode (parallel nod and
;                 chop with equal amplitudes. When this mode is detected
;                 DRIP bypasses the 'MERGE' step in the pipeline.
;   Modifed:      Luke Keller, Ithaca College, June 2011
;                 Added 'C2NC2' mode
;   Modified:     Luke Keller, Ithaca College, May 2012
;                 Added grism spectra coadd option
;               
;******************************************************************************
;     DRIP_COADD - Merges data frames
;******************************************************************************

function drip_coadd, newdata, coadded, header, basehead, first=first, n=n, radec=radec

; Check for GRISM spectral mode
specmode = drip_getpar(basehead,'ICONFIG')
    
; For an extracted spectrum no image manipulation is necessary, just coadd 
; (1D) spectra.
;specmode = 'X' ; Uncomment to turn off spectrum coadd for testing
if (specmode eq 'SPECTROSCOPY') then begin ;GRISM
   ; GRISM spectra coadd
   drip_message,'drip_coadd: Coadding grism spectra'
   if keyword_set(first) then begin 
      newcoadded=newdata
         
   endif else begin
      ; Data from *self.extracted are: wavelength, flux, flux_error, spectral_order
      ; coadd flux only
      if (keyword_set(n) eq 1) then begin
          ; This is at least the third spectrum to be coadded
          newcoadded=[ [coadded[*,0]],[coadded[*,1]*(n-1)+newdata[*,1]/(n)],$
              [coadded[*,2]],[coadded[*,3]] ]
          
          ;newcoadded=newdata
      endif else begin
          ; This is the second spectrum to be coadded
          newcoadded=[ [coadded[*,0]],[(coadded[*,1]+newdata[*,1])/2],$
              [coadded[*,2]],[coadded[*,3]] ]
          
          ;newcoadded=newdata 
      endelse  
      ; NEED TO ADD ERROR CHECK: coadded wave and order should be = newdata wave and order
       
   endelse    
      
endif else begin   ; IF not GRISM, then process as IMAGE

; error check

mode=drip_getpar(header,'INSTMODE')
if (uint(sxpar(header, 'C2NC2')) eq 1) then mode='C2NC2'

mode=strtrim(mode,2)

s=size(newdata)
if s[0] ne 2 then begin
    if (mode eq 'C2NC2') then begin ; Check for correct C2NC2 data format
      if s[0] ne 3 then begin
        ; C2NC2 data are cubes of 8 frames
        drip_message, 'drip_coadd - invalid C2NC2 data - aborting', /fatal
	sxaddpar,basehead,'HISTORY','Coadd was not apply (Invalid data)'
      endif   
    endif else begin
      drip_message, 'drip_coadd - invalid new data array - aborting',/fatal
      sxaddpar,basehead,'HISTORY','Coadd was not apply (Invalid data)'
      return, newdata
    endelse
endif
cs=size(coadded)
if cs[0] ne 2 then begin
    drip_message, 'drip_coadd - no previous coadded array - using blank image'
    coadded=newdata
    coadded[*,*]=0.0
endif
hs=size(header)
if (hs[0] ne 1) or (hs[2] ne 7) then drip_message, $
  'drip_coadd - invalid header'
hs=size(basehead)
if (hs[0] ne 1) or (hs[2] ne 7) then drip_message, $
  'drip_coadd - invalid base header'
; initialize variables
newcoadded=fltarr(s[1],s[2])  

odata = newdata ; Preserve original data

; Get data for rotation of field using SKY_ANGLE FITS keyword
skyangle=drip_getpar(header,'SKY_ANGL') ; GET SKY ANGLE FOR ROTATION OF FIELD; header
skyangle=strtrim(skyangle,2)
;rot_angle = 180. - skyangle
rot_angle= 0

resizeread = drip_getpar(basehead,'RESIZE')
if resizeread eq 'x' then resize=2.0 $
else resize = float(resizeread)
borderread = drip_getpar(basehead,'BORDER')
if borderread eq 'x' then border=128 $
else border = fix(borderread)
xyshiftread = drip_getpar(basehead,'XYSHIFT')
if xyshiftread eq 'x' then xyshift=15. $
else xyshift = fix(xyshiftread)

xyplate_scale = 0.768

; run appropriate method
switch mode of
    'C2': ; 2 position chop
    'C2N':begin ; 2 position chop with nod
     
        xdither = 0.0
        ydither = 0.0
        dithermode = sxpar(header,'DITHERS')
	
	; Data is rotate at the merge step for C2N
	rot_angle= 0.
	
        if (uint(sxpar(header,'DITHERS')) eq 1) then begin
            ; Get chop coord system: 0=SIRF, 1=TARF, 2=ERF
            dcoordsys_read=drip_getpar(header,'DITHERCS')
            dcoordsys_read=strtrim(dcoordsys_read,2)
            if dcoordsys_read eq 'x' then dcorrdsys = 2 $
	    else dcoordsys=fix(dcoordsys_read)
            xdither = drip_getpar(header,'DITHERX')
            xdither = float(strtrim(xdither,2))
            ydither = drip_getpar(header,'DITHERY')
            ydither = float(strtrim(ydither,2))
            ; We do not rotate anymore (xdither,ydither) in the case
	    ; of ERF coordinate system because the images are already
	    ; north oriented
	    ;if (dcoordsys eq 2) then begin
            ;  erf_xdither = (xdither*cos(skyangle) + ydither*sin(skyangle))
            ;  erf_ydither = (ydither*cos(skyangle) - xdither*sin(skyangle))
            ;  xdither = erf_xdither
            ;  ydither = erf_ydither
            ;endif
	    
	    ; We calculate xdither and ydither in the sky coordinate system
	    ; when their values are written in SIRF
            if (dcoordsys eq 0) then begin
              sirf_xdither = (xdither*cos(skyangle) + ydither*sin(skyangle))
              sirf_ydither = (-ydither*cos(skyangle) - xdither*sin(skyangle))
              xdither = sirf_xdither
              ydither = sirf_ydither
            endif
        endif
         
        if keyword_set(first) then begin 
            newcoadded=newdata             
        endif else begin
            
            corcoadd=drip_getpar(header,'CORCOADD')
            
            newrez=newdata[border:s[1]-border-1,border:s[2]-border-1]
            newcoadd = coadded[border:s[1]-border-1,border:s[2]-border-1]
            
            ;newrez = rot(newrez, rot_angle, cubic=-0.5, missing=0) ; Rotate new data using SKY_ANGL
            rot_newdata = newdata
	    
            ;** add frames (maybe cross-correlate)
            ; Keyword replace flag for merge method:
            ;  CORCOADD = 'COR' then use cross-correlation
            ;  CORCOADD = 'CENT' then use centroid
            ;  CORCOADD = 'N' then use nominal nod and chop positions
            SWITCH corcoadd of
            ;if (CORCOADD eq 'COR' ) OR (CORCOADD eq 'CORFLT' ) then begin ; OR CORCOADD eq 'CENT' 
              'CORFLT':
	      'COR': begin
        	     ;xyshift=15
        	     drip_message,'drip_coadd - Using cross-correlation to coadd frames'
        	     cmat=correl_images(newcoadd, newrez, xoff=-xdither, yoff=ydither, xshift=xyshift, yshift=xyshift, $
                	reduction=8) ;was xshift=20, yshift =20
        	     corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=-xdither, YOFF_INIT=ydither
		     
		     xopt2=xopt
        	     yopt2=yopt
        	     ;xyshift=10
        	     cmat=correl_images(newcoadd, newrez, xoff=xopt2, yoff=yopt2, xshift=xyshift, yshift=xyshift)
        	     corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=xopt2, YOFF_INIT=yopt2
		     
        	     newshift=shift(rot_newdata,xopt,yopt) ;newrez
		     sxaddpar,basehead,'HISTORY','Coadd: X,Y shift of image is '+strtrim(xopt,1)+','+strtrim(yopt,1)
                     break
	      end
	    ;endif
            ;if CORCOADD eq 'CENT' then begin ; Use centroids to align images
              'CENT': begin   
        	      drip_message,'drip_coadd - Using centroid to coadd frames'
        	      threshread = drip_getpar(basehead,'CTHRESH')
		      if threshread eq 'x' then thresh=15.0 $
		      else thresh = float(threshread)

        	      shift_coords = drip_peakfind(newcoadd,newrez,fwhm=6.0,npeaks=1, thresh=thresh,/showatv) ;newrez
        	      newshift = shift(rot_newdata,shift_coords[0],shift_coords[1])  ; newrez
		      sxaddpar,basehead,'HISTORY','Coadd: X,Y shift of image is '+strtrim(shift_coords[0],1)+','+strtrim(shift_coords[1],1)
        	      break
              end
	    ;endif
            ;if CORCOADD eq 'N' then begin;  ne 'CENT' and CORCOADD ne 'COR' then begin
              'N': begin   
        	   xopt = -xdither
        	   yopt = ydither
		   newshift=shift(rot_newdata,xopt,yopt)
        	   ;print, 'Shift/register using HK and DITHER'
		   sxaddpar,basehead,'HISTORY','Coadd: X,Y shift of image is '+strtrim(xopt,1)+','+strtrim(yopt,1)
        	   break
              end
            ;endif
              else: begin
	            newshift = rot_newdata
		    break
	      end
	    ENDSWITCH
	    
            if (keyword_set(n) eq 1) then begin
                newcoadded=(coadded*(n)+newshift)/(n+1) ; mean
            endif else begin
                newcoadded=(coadded+newshift)/2
            endelse
            
        endelse
        break
    end
    'C2NC2': begin ; 2 position large asymmetric chop with large offset
        rot_angle = 180. - skyangle
        corcoadd=drip_getpar(header,'CORCOADD')

        ; Incorporate dither as first guess for registration of images
        xdither = 0.0
        ydither = 0.0
        dithermode = sxpar(header,'DITHERS')
        ; print, 'DITHER:  ',dithermode
        if (uint(sxpar(header,'DITHERS')) eq 1) then begin
            ; Get chop coord system: 0=SIRF, 1=TARF, 2=ERF
            dcoordsys_read=drip_getpar(header,'DITHERCS')
            dcoordsys_read=strtrim(dcoordsys_read,2)
            if dcoordsys_read eq 'x' then dcorrdsys = 2 $
	    else dcoordsys=fix(dcoordsys_read)
            xdither = drip_getpar(header,'DITHERX')
            xdither = float(strtrim(xdither,2))
            ydither = drip_getpar(header,'DITHERY')
            ydither = float(strtrim(ydither,2))
	    ; xdither and ydither are used when corcoadd='N'
	    ; We do not rotate their values when erf coordinate system
	    ;if (dcoordsys eq 2) then begin
            ;  erf_xdither = (xdither*cos(skyangle) + ydither*sin(skyangle))
            ;  erf_ydither = (ydither*cos(skyangle) - xdither*sin(skyangle))
            ;  xdither = erf_xdither
            ;  ydither = erf_ydither
            ;endif
            ; print,'DITHERX:  ',xdither,'  DITHERY:  ',ydither
	    
	    ; Then we divide the dither by the plate scale since
	    ; the values in the headers are in arcseconds
	    xdither = xdither/xyplate_scale
            ydither = ydither/xyplate_scale
        endif
        
        ; Data will come from merge in a cube mergestack
          
        ; Remove border and resize       
        newrez=newdata[border:s[1]-border-1,border:s[2]-border-1,*]
        
        ; Subtract ON-OFF
        
        sky_sub = newrez[*,*,0]-newrez[*,*,1]            ; Use for cross-corr
        sky_sub_data = newdata[*,*,0] - newdata[*,*,1]   ; Use for shift
        
        if keyword_set(first) then begin   
            
	    if corcoadd eq 'N' and (dcoordsys ne 2) then $
	      newshift=shift(sky_sub_data[*,*],-xdither,ydither)
	      
            newcoadded = rot(sky_sub_data, rot_angle, cubic=-0.5,missing=0) ; Rotate new data
            drip_message, ['Rotated frame to North = UP using SKY_ANGL = ', skyangle]
            
	    if corcoadd eq 'N' and (dcoordsys eq 2) then $
	      newshift=shift(sky_sub_data[*,*],-xdither,ydither)
	      
        endif else begin
	    CASE corcoadd of
	      'CENT' : begin
	    ;if corcoadd eq 'CENT' then begin ; Use centroids to align images
        	       ; use to find the shift of the image
        	       newcoadd = coadded[border:s[1]-border-1,border:s[2]-border-1]

        	       sky_sub = rot(sky_sub, rot_angle, cubic=-0.5,missing=0) ; Rotate new data
        	       sky_sub_data = rot(sky_sub_data, rot_angle, cubic=-0.5,missing=0)

        	       drip_message,'drip_coadd - Using centroid to coadd frames'
		       threshread = drip_getpar(basehead,'CTHRESH')
		       if threshread eq 'x' then thresh=15.0 $
		       else thresh = float(threshread)

        	       shift_coords = drip_peakfind(newcoadd,sky_sub,fwhm=6.0,npeaks=1, thresh=thresh,/showatv) 
		       
        	       newshift = shift(sky_sub_data,shift_coords[0],shift_coords[1])  ; newrez
		       sxaddpar,basehead,'HISTORY','Coadd: X,Y shift of image is '+strtrim(shift_coords[0],1)+','+strtrim(shift_coords[1],1)
        	       ;newshift=origin
        	       ;corcoaddd=origin
        	       ;newcoadded=coadded+r
            ;endif else begin    
                       break
	               end
	      'COR' : begin
        	      ; use to find the shift of the image
        	      newcoadd = coadded[border:s[1]-border-1,border:s[2]-border-1]

        	      sky_sub = rot(sky_sub, rot_angle, cubic=-0.5,missing=0) ; Rotate new data
        	      sky_sub_data = rot(sky_sub_data, rot_angle, cubic=-0.5,missing=0)
		      if keyword_set(radec) eq 0 then begin
                	 ;xyshift=15
        	      endif else begin
                	 ;xyshift=15
                	 ; This is more complex than I though we need to store the position of
                	 ; the telescope (RA,DEC) for the first image coadded. But the first image
                	 ; containing the source so we need to check out which of the first pair is
	        	 ; Same thing for the current pair, we need to parse (ra, dec) for the image
	        	 ; with the source which is not necessarily the one in header. It is possible 
	        	 ; but I do not want to screw the things now before the reduction of basic science
	        	 ; Better we take more processing time using a large correlation box
	        	 ;telra=sxpar(header,'TELRA')
	        	 ;teldec=sxpar(header,'TELDEC')
	        	 ;deltara = telra - radec[0]
	        	 ;deltadec = teldec - radec[1]
	        	 ; Measured pixel scales from Jim De Buizer
	        	 ;xpixsc = 0.787
	        	 ;ypixsc = 0.745
		      endelse

        	      cmat=correl_images(newcoadd[5:250,5:250], sky_sub[5:250,5:250], xoff=-xdither, yoff=ydither, xshift=xyshift, $
                	  yshift=xyshift, reduction=4)
        	      corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=-xdither, YOFF_INIT=ydither

        	      xopt2=xopt
        	      yopt2=yopt
        	      ;xyshift=15

        	      cmat=correl_images(newcoadd[5:250,5:250], sky_sub[5:250,5:250], xoff=xopt2, yoff=yopt2, xshift=xyshift, yshift=xyshift)
        	      corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=xopt2, YOFF_INIT=yopt2
        	      newshift=shift(sky_sub_data[*,*],xopt,yopt)
        	      sxaddpar,basehead,'HISTORY','Coadd: X,Y shift of image is '+strtrim(xopt,1)+','+strtrim(yopt,1)                     
            ;endelse   
	              break
		      end                           
              'N' : begin
		      xopt = -xdither
	              yopt = ydither
		      ; if sky coordinate system we rotate the image then we shift
	              ; if instrument coordinate system we shift by xdither and ydither then we rotate
		      if (dcoordsys eq 2) then begin
		        ; rotate image
			sky_sub_data = rot(sky_sub_data, rot_angle, cubic=-0.5,missing=0)
			
			;shift image
			newshift=shift(sky_sub_data[*,*],xopt,yopt)
		      endif else begin
		        ; shift image
			newshift=shift(sky_sub_data[*,*],xopt,yopt)
			
			; rotate image
			sky_sub_data = rot(sky_sub_data, rot_angle, cubic=-0.5,missing=0)			
		      endelse
        	      sxaddpar,basehead,'HISTORY','Coadd: X,Y shift of image is '+strtrim(xopt,1)+','+strtrim(yopt,1) 	            
		    break
	            end	    
	      else: newshift = sky_sub_data
	    ENDCASE
	    
            if (keyword_set(n) eq 1) then begin
                newcoadded=(coadded*(n-1)+newshift)/(n) ; mean
            endif else begin
                newcoadded=(coadded+newshift)/2
            endelse   
        endelse
      break
    end
    'C2ND': ; C2N with Dither
    'C3D':begin ; 3 position chop with Dither
        ;** shift image from RA, DEC
        ; get original and new ra/dec (ra in hours, dec in degs)
        ;   convert all to degrees
        basera=15.0*float(drip_getpar(basehead,'TELRA'))
        basedec=float(drip_getpar(basehead,'TELDEC'))
        newra=15.0*float(drip_getpar(header,'TELRA'))
        newdec=float(drip_getpar(header,'TELDEC'))
        avgdec=(basedec+newdec)/2.0
        ; make correction for nodding
        if mode eq 'C2ND' then begin
            nodbeam=strtrim(drip_getpar(header,'NODBEAM'),2)
            if nodbeam eq 'B' then begin
                newra=newra-float(drip_getpar(header,'NODRAAS'))/3600.0/ $
                  cos(!pi/180.0*avgdec)
                newdec=newdec-float(drip_getpar(header,'NODDECAS'))/3600.0
            endif
        endif
        raoff=(newra-basera)*cos(!pi/180.0*avgdec)
        decoff=newdec-basedec
        ; convert to seconds then pixels (remember 2x subsampled to match coadd frame format)
        ;arcsecppix=0.43 ; palomar 2007
        arcsecpix=0.75 ; assume telescope is SOFIA
        telescope=drip_getpar(header,'TELESCOP') ; to determine plate scale
        if telescope eq 'PIXELS' then arcsecpix=1.0
        
        raoff=resize*3600.0*raoff/arcsecppix
        decoff=resize*3600.0*decoff/arcsecppix
        ; shift image (raoff left, decoff up)    
        
        ;print,'raoff=',raoff,' decoff=',decoff
        ;newshift=shift(newdata,-raoff,decoff)
        
        ;** shift image from DITHERX and DITHERY (remember 2x subsampled to match coadd frame format)
        ditherx=resize*(drip_getpar(header,'DITHER_X'))
        dithery=resize*(drip_getpar(header,'DITHER_Y'))
        ;print,'ditherx=',ditherx,' dithery=',dithery
        newshift=shift(newdata,-ditherx,-dithery)
        ; add image to last coadded images
        if keyword_set(first) then newcoadded=newshift $
          else newcoadded=(coadded+newshift)/2  ; Save and display mean
        break
    end
    'CM':begin ; multi position chop
        if keyword_set(first) then newcoadded=newdata $
          else newcoadded=coadded+newdata
        break
    end
    'MAP':begin ; MAPping mode
        ;** new map: set size of final map and make map
        nx=fix(drip_getpar(header,'MAPNXPOS'))
        ny=fix(drip_getpar(header,'MAPNYPOS'))
        posx=fix(drip_getpar(header,'MAPPOSX'))-1
        posy=fix(drip_getpar(header,'MAPPOSY'))-1
        intx=fix(drip_getpar(header,'MAPINTX'))
        inty=fix(drip_getpar(header,'MAPINTY'))
        sizex=256+(nx-1)*intx
        sizey=256+(ny-1)*inty
        ;print,sizex,sizey
        if keyword_set(first) then newcoadded=fltarr(sizex,sizey) $
          else newcoadded=coadded
        newx=posx*intx
        newy=posy*inty
        newcoadded[newx:newx+255,newy:newy+255]=newdata
        break
    end
    'STARE':begin ; STARE
        if keyword_set(first) then newcoadded=newdata $
          else newcoadded=newrez+coadded ; newdata-->newrez
        break
    end
    'TEST':begin ; TEST
        submode=drip_getpar(header,'TESTCADD')
        switch submode of
            ; add frames
            'ADD': begin
                if keyword_set(first) then newcoadded=newdata $
                  else newcoadded=newdata+coadded
                break
            end
            ; subtract each other frame
            'SUB': begin
                if keyword_set(first) then newcoadded=newdata $
                  else newcoadded=newdata-coadded
                break
            end
            ; add A's subtract B's
            'ABBA': begin ; use n, add A, subtract B
                if keyword_set(first) then newcoadded=newdata $
                  else if (n+1) mod 4 lt 2 then newcoadded=coadded+newdata $
                    else newcoadded=coadded-newdata
                break
            end
            ; keep all frames (3D image) use first as background
            'LIST_BACKSUB': begin
                if keyword_set(first) then newcoadded=newdata $
                  else begin
                    newsub=newdata-coadded[*,*,0]
                    newcoadded=[[[coadded]],[[newsub]]]
                endelse
                break
            end
            ; full nodding
            'NOD': begin ; use n, chop according to ABBA
                ; get nod distances
                noddist=float(drip_getpar(header,'NODAMP'))
                nodang=float(drip_getpar(header,'NODANGLE'))
                noddist=float(noddist)
                nodang=!pi/180*nodang
                nodx=noddist*sin(nodang)
                nody=-noddist*cos(nodang)
                ; shift and coadd
                if keyword_set(first) then begin
                    newcoadded=newdata-shift(newdata,nodx,nody)
                endif else begin
                    if (n+1) mod 4 lt 2 then begin
                        newcoadded=newdata-shift(newdata,nodx,nody)+coadded
                    endif else begin
                        newcoadded=shift(newdata,nodx,nody)-newdata+coadded
                    endelse
                endelse
                break
            end
            else: if keyword_set(first) then newcoadded=newdata $
              else newcoadded=newdata-coadded
        endswitch
        break
    end
    else:begin
        drip_message, ['drip_coadd - invalid instrument mode', $
                       '  ignoring new data']
        newcoadded=coadded
    endelse
endswitch

newdata = odata ; Set newdata back to original since it is a pointer
                ; to data used elsewhere (e.g. in drip_merge)

endelse ; ELSE started at GRISM

return, newcoadded  ; newcoadded

end
