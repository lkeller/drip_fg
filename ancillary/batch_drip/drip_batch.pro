pro drip_batch,path

; DRIP script to reduce FORCAST images
; L. Keller 29 November 2010
; Load drip configuration file
conffilename='/Users/keller/Research/drip_github/DRIP/work/dripconf.txt'
drip_config_load,conffilename=conffilename
; Create the drip object to reduce SWC
; using file list
;filelist=dialog_pickfile(filter=path, $
;      /fix_filter, /must_exist, /read, /multiple_files,title='Files to reduce:' )

filelist=['/Users/forcast/Public/data/FT05/B/bFT05_0322.fits', $
'/Users/forcast/Public/data/FT05/B/bFT05_0323.fits', $
'/Users/forcast/Public/data/FT05/B/bFT05_0324.fits', $
'/Users/forcast/Public/data/FT05/B/bFT05_0325.fits',$
'/Users/forcast/Public/data/FT05/B/bFT05_0326.fits',$
'/Users/forcast/Public/data/FT05/B/bFT05_0327.fits']

for i=0,n_elements(filelist)-1 do begin
    reduce_obj=drip_new(filelist[i])
    ; Run the pipeline
    reduce_obj->run, filelist[i]
    ; save reduced data _reduced, _stacked, _undistor
    namepos=strpos(filelist[i],'.fit',/reverse_search)
    reduce_obj-> save, filename=strmid(filelist[i],0,namepos)+'_reduced.fits'
    
    ;filename=(*self.filelist)[self.ind-1]
    namepos=strpos(filelist[i],'.fit',/reverse_search)
    reduce_obj-> save, filename=strmid(filelist[i],0,namepos)+'_stacked.fits',$
                       /stacked
                       
    ;filename=(*self.filelist)[self.ind-1]
    namepos=strpos(filelist[i],'.fit',/reverse_search)                   
    reduce_obj-> save, filename=strmid(filelist[i],0,namepos)+'_undistort.fits',$
                       /undistort
    ; destroy the object
    obj_destroy, reduce_obj
endfor

print,'DRIP FINISHED'

end
