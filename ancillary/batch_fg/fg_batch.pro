
pro fg_batch,file_list,merged=merged

; Reduces and saves a list of FORCAST Grism raw data files

; Uncomment to enable a user interactive file
; selection widget

;filelist=dialog_pickfile(filter='/Users/forcast/Public/data/FT04/', $
;      /fix_filter, /must_exist, /read,
;      /multiple_files,title='FORCAST files to reduce:' )

; file_list should be a text file with a single column containing
; paths to files for reduction

; OR hard code the list here in the following format

;['/Users/forcast/Public/data/FT03/Btest/bFT03_0212.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0213.fits', $
;'/Users/forcast/Public/data/FT03/Btest/bFT03_0214.fits']

; Load pipeline configuration file

conffilename='/home/keller/SOFIA/grism/FG/FG_Widget/dripconf.txt'
drip_config_load,conffilename=conffilename

; Reduce files in the file_list
; The following reduces and save the files in the list in the
; same pipeline (coadded)

redux=drip_new(file_list)

redux->run, file_list

; Save reduced data
;namepos=strpos(file_list[i],'.fits',/reverse_search)
; To specify filename add: filename=strmid(file_list[i],0,namepos)+'_reduced.fits'
    
if keyword_set(merged) then begin
    redux-> save,/merged
endif else begin
    redux-> save
endelse

obj_destroy, redux

; The following reduces and saves each file in file_list separately
; (no coadding)
;for i=0,n_elements(file_list)-1 do begin

    ; Create a pipeline object

    ;redux=drip_new(file_list[i])

    ; Run the pipeline

    ;redux->run, file_list[i]

    ; Save reduced data
    ;namepos=strpos(file_list[i],'.fits',/reverse_search)
    ; To specify filename add: filename=strmid(file_list[i],0,namepos)+'_reduced.fits'
    
    ;if keyword_set(merged) then begin
    ;    redux-> save,/merged
    ;endif else begin
    ;    redux-> save
    ;endelse

    ; Destroy the object

    ;obj_destroy, redux

;endfor

print,'FG FINISHED'


end
