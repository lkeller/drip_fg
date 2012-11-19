function c2nc2,rb,imnuma,imnumb,ignore, return_median=return_median, cube=cube,no_reduce=no_reduce

; Reduces and combines five dittered observations with off-chip chop and nod
; taken in C2 mode with the pattern: O-S-O-O-S-O-O-S (ISF flight FT05 -->)


;filelist=dialog_pickfile(filter='/Users/forcast/Public/data/FT04/', $
;      /fix_filter, /must_exist, /read, /multiple_files,title='C2C2 files to reduce:' )

;['/Users/forcast/Public/data/FT03/Btest/bFT03_0212.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0213.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0214.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0215.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0216.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0217.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0218.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0219.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0220.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0221.fits']

filelist=strarr(imnumb-imnuma+1)
reducelist=strarr(imnumb-imnuma+1)

if (rb eq 'b') then begin
    conffilename='/Users/keller/Research/drip_github/DRIP/work/dripconf.txt'
    drip_config_load,conffilename=conffilename
    fpath='/Users/forcast/Public/data/FT06/B/'
    imindex=imnuma
    for i=0,n_elements(filelist)-1 do begin
        if (imindex+i eq ignore) then begin
            imindex+=1  ;skip this file number
        endif
        filelist[i]=fpath+strtrim('bFT06_0')+strtrim(string(imindex+i),2)+'.fits'
        reducelist[i]=fpath+strtrim('bFT06_0')+strtrim(string(imindex+i),2)+'_reduced.fits'
    endfor
endif

if (rb eq 'r') then begin
    conffilename='/Users/keller/Research/drip_github/DRIP/work/lwc_dripconf.txt'
    drip_config_load,conffilename=conffilename
    fpath='/Users/forcast/Public/data/FT06/R/'
    imindex=imnuma
    for i=0,n_elements(filelist)-1 do begin  
        if (imindex+i eq ignore) then begin
            imindex+=1  ;skip this file number
        endif
        filelist[i]=fpath+strtrim('rFT06_0')+strtrim(string(imindex+i),2)+'.fits'
        reducelist[i]=fpath+strtrim('rFT06_0')+strtrim(string(imindex+i),2)+'_reduced.fits'
    endfor
endif 

; Send data to DRIP for reduction and save reduced files 
if not keyword_set(no_reduce) then begin

for i=0,n_elements(filelist)-1 do begin

    redux=drip_new(filelist[i])

    ; Run the pipeline

    redux->run, filelist[i]

    ; Save reduced data

    redux-> save

    ; Destroy the object

    obj_destroy, redux

endfor

print,'DRIP FINISHED'

endif



; Subtract and register chop/nod sequences
a=fltarr(256,256,n_elements(filelist))
b=fltarr(256,256,n_elements(filelist))

;print,reducelist

a[*,*,0]=readfits(reducelist[0],hdr)  ; read data and get header for saving later
a[*,*,1]=readfits(reducelist[1])
a[*,*,2]=readfits(reducelist[2])
a[*,*,3]=readfits(reducelist[3])
a[*,*,4]=readfits(reducelist[4])
a[*,*,5]=readfits(reducelist[5])
a[*,*,6]=readfits(reducelist[6])
a[*,*,7]=readfits(reducelist[7])


;sky_sub_first = a-b
sky_sub=fltarr(256,256,n_elements(filelist)-3)
sky_sub[*,*,0]=a[*,*,0]-a[*,*,1]
sky_sub[*,*,1]=a[*,*,2]-a[*,*,1]
sky_sub[*,*,2]=a[*,*,3]-a[*,*,4]
sky_sub[*,*,3]=a[*,*,5]-a[*,*,4]
sky_sub[*,*,4]=a[*,*,6]-a[*,*,7]

;writefits,flightpath+image_a+'_skysub.fits',sky_sub,header

;atv22,sky_sub[0]  ;_first

; Now coadd and register dithered frames

; cormerge=drip_getpar(header,'CORMERGE')
num=5  ;n_elements(filelist)/2+1
combine_cube=fltarr(256,256,num)
combine_cube[*,*,0]=sky_sub[*,*,0]
coadded=sky_sub[*,*,0]

if (num gt 1) then begin
;for i=0,num-2,2 do begin
for i=1,4 do begin

;a=readfits(reducelist[i])   ;(flightpath+image_a+'_reduced.fits', header)
;b=readfits(reducelist[i+1]) ;(flightpath+image_b+'_reduced.fits')

;sky_sub = a-b
;atv22,sky_sub[*,*,i]

cormerge='COR'                                                   
if (cormerge eq 'COR' ) then begin
    xyshift=25
    ;drip_message,'drip_coadd - Using cross-correlation to coadd frames'
    ;cmat=correl_images(coadded, sky_sub, xoff=0, yoff=0, xshift=xyshift, yshift=xyshift, $
    ;             reduction=4) ;was xshift=20, yshift =20
    ;corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=0, YOFF_INIT=0, reduction=4
    ;xopt2=xopt
    ;yopt2=yopt
    
    cmat=correl_images(sky_sub[5:250,5:250,0], sky_sub[5:250,5:250,i], xoff=xopt2, yoff=yopt2, xshift=xyshift, yshift=xyshift, $
                 reduction=2) ;was xshift=20, yshift =20
    corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=0, YOFF_INIT=0,reduction=2
    print,xopt,yopt
    xopt2=xopt
    yopt2=yopt
               
    cmat=correl_images(sky_sub[5:250,5:250,0], sky_sub[5:250,5:250,i], xoff=xopt2, yoff=yopt2, xshift=xyshift, yshift=xyshift)
    corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=xopt2, YOFF_INIT=yopt2
    print,xopt,yopt
    newshift=shift(sky_sub[*,*,i],xopt,yopt)
endif

; Coadd

coadded+=newshift

combine_cube[*,*,i]=newshift
endfor
endif

s=size(combine_cube)

if (s[0] lt 3) then begin
    print, 'Returning single image'
    atv22,combine_cube
    return,combine_cube
endif

if keyword_set(cube) then begin
   
    atv22,combine_cube
    print,'C2NC2 FINISHED:  Returing data cube'
    return,combine_cube
endif
    

if (s[0] gt 2) then begin 
    median_combined = median(combine_cube,dimension=3)
    mean_combined = total(combine_cube,3)/s[3]
    if keyword_set(return_median) then begin
        atv22,median_combined
        print,'C2NC2 FINISHED:  Returning median combined image'
        if (rb eq 'b') then begin
        writefits,fpath+strtrim('bFT06_0')+strtrim(string(imnuma),2)+'-'+ $
                                 strtrim(string(imnumb),2)+'_medcomb.fits',median_combined,hdr
        endif
        if (rb eq 'r') then begin
        writefits,fpath+strtrim('rFT06_0')+strtrim(string(imnuma),2)+'-'+ $
                                 strtrim(string(imnumb),2)+'_medcomb.fits',median_combined,hdr
        endif
        return, median_combined 
    endif else begin    
        print, 'C2NC2 FINISHED:  Returning mean combined image'
        atv22, mean_combined
        if (rb eq 'b') then begin
        writefits,fpath+strtrim('bFT06_0')+strtrim(string(imnuma),2)+'-'+ $
                                 strtrim(string(imnumb),2)+'_meancomb.fits',mean_combined,hdr
        endif
        if (rb eq 'r') then begin
        writefits,fpath+strtrim('rFT06_0')+strtrim(string(imnuma),2)+'-'+ $
                                 strtrim(string(imnumb),2)+'_meancomb.fits',mean_combined,hdr
        endif
        return, mean_combined
    endelse
endif    

end