; NAME:
;     FIND_DRIPCONFIG - Version 1.1
;
; PURPOSE:
;     Browse directories in idl path (!PATH) and look for dripconf.txt
;     Then, checks the format of the file and returns the first 
;     file which matches the format
;
; CALLING SEQUENCE:
;     FILENAME = FIND_DRIPCONFIG()
;
; INPUTS:
;     
; OUTPUTS:
;     FILENAME: complete path name of the file if found or '' if not 
;
; CALLED ROUTINES AND OBJECTS:
;
; SIDE EFFECTS:
;     None
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos Llorens, USRA, 2012
;


FUNCTION find_dripconf, debug=debug, guiconf=guiconf
  
  path_array = strsplit(!PATH , ':', /extract)
  fname = 'dripconf.txt'
  if keyword_set(guiconf) then fname = 'guiconf.txt'
  
  for i=0, n_elements(path_array)-1 do begin
    if keyword_set(debug) then print,'Checking '+path_array[i]+'...'
    farr = findfile(path_array[i]+'/'+fname)
    if keyword_set(debug) then print,farr
    if farr[0] ne '' then begin
      for j=0,n_elements(farr)-1 do begin
        ; Here we should check the format but for now I am going to assume
	; that it is always correct if found
	print, 'File found: '+farr[0]
	return, farr[0]
      endfor
    endif 
  endfor
  
  print,'File not found'
  
  return,''
  
END
