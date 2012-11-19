function c2c2 ;  image_a, image_b
;image_a and image_b are REDUCED C=(C2 mode) images of ON (a) and
;OFF (b) source observations

; c2c2 returns the difference between the two images

flightpath='/Users/forcast/Public/data/FT03/Btest/'

file_list=dialog_pickfile(filter='/Users/forcast/Public/data/FT04/', $
      /fix_filter, /must_exist, /read, /multiple_files,title='C2C2 files to reduce:' )
;print,file_list
num = n_elements(file_list)

a=readfits(file_list[0])   ;(flightpath+image_a+'_reduced.fits', header)
b=readfits(file_list[1])   ;(flightpath+image_b+'_reduced.fits')

sky_sub_first = a-b

;writefits,flightpath+image_a+'_skysub.fits',sky_sub,header

atv22,sky_sub_first

; Now coadd and register dithered frames

; cormerge=drip_getpar(header,'CORMERGE')

combine_cube=fltarr(256,256,num/2)
combine_cube[*,*,0]=sky_sub_first
coadded=sky_sub_first

for i=2,num-2,2 do begin

a=readfits(file_list[i])   ;(flightpath+image_a+'_reduced.fits', header)
b=readfits(file_list[i+1]) ;(flightpath+image_b+'_reduced.fits')

sky_sub = a-b
atv22,sky_sub

cormerge='COR'                                                   
if (cormerge eq 'COR' ) then begin
    xyshift=25
    ;drip_message,'drip_coadd - Using cross-correlation to coadd frames'
    ;cmat=correl_images(coadded, sky_sub, xoff=0, yoff=0, xshift=xyshift, yshift=xyshift, $
    ;             reduction=4) ;was xshift=20, yshift =20
    ;corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=0, YOFF_INIT=0, reduction=4
    ;xopt2=xopt
    ;yopt2=yopt
    
    cmat=correl_images(sky_sub_first[5:250,5:250], sky_sub[5:250,5:250], xoff=xopt2, yoff=yopt2, xshift=xyshift, yshift=xyshift, $
                 reduction=2) ;was xshift=20, yshift =20
    corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=0, YOFF_INIT=0,reduction=2
    print,xopt,yopt
    xopt2=xopt
    yopt2=yopt
               
    cmat=correl_images(sky_sub_first[5:250,5:250], sky_sub[5:250,5:250], xoff=xopt2, yoff=yopt2, xshift=xyshift, yshift=xyshift)
    corrmat_analyze, cmat, xopt, yopt, XOFF_INIT=xopt2, YOFF_INIT=yopt2
    print,xopt,yopt
    newshift=shift(sky_sub,xopt,yopt)
endif
;if cormerge eq 'CENT' then begin ; Use centroids to align images
;    ; drip_message,'drip_coadd - Using centroid to coadd frames'
;    ;centdata = odata[*,0:254]
;    newshift=find_peak_all(sky_sub)   ; was newdata
;endif
;if cormerge ne 'CENT' and cormerge ne 'COR' then begin
;    newshift=sky_sub
;endif

; Coadd

coadded+=newshift

j=i/2
combine_cube[*,*,j]=newshift
endfor

s=size(combine_cube)

if (s[0] gt 2) then begin 
    median_combined = median(combine_cube,dimension=3)
    print,'Returning median combined image'
    atv22,median_combined
    return, total(combine_cube,3)/s[3]; combine_cube;median_combine  
endif else begin 
    atv22,combine_cube
    return,combine_cube
endelse
print,'FINISHED'    
end