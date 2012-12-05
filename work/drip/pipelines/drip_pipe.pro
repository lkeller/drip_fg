FUNCTION SaveProduct, drip_obj, fname, foutarr=foutarr, outlist=outlist, nosave=nosave
  
  if keyword_set(foutarr) then resarr = foutarr
  
  namepos=strpos(fname,'.fit',/reverse_search)
  rootname = strmid(fname,0,namepos)
  
  ; Save product and record its name in the output manifest
  if not keyword_set(nosave) then drip_obj->saveproducts,'ALL',output='cube'
  namepos=strpos(fname,'.fit',/reverse_search)
  outfname = rootname + "_all.fits"
  if FILE_TEST(outfname) eq 1 then begin
    print,'Saved file '+outfname
    if n_elements(resarr) eq 0 then resarr = [outfname] else resarr = [resarr,outfname]
  endif
  
  if keyword_set(outlist) then begin
    if outlist[0] ne '' then begin
      if not keyword_set(nosave) then drip_obj->saveproducts,outlist
      namepos=strpos(fname,'.fit',/reverse_search)

      for i=0,n_elements(outlist)-1 do begin
	outfname = rootname + "_"+strlowcase(outlist[i])+".fits"
	if FILE_TEST(outfname) eq 1 then begin
	  print,'Saved file '+outfname
	  if n_elements(resarr) eq 0 then resarr = [outfname] else resarr = [resarr,outfname]
	endif
      endfor
    endif
  endif
  
  if n_elements(resarr) eq 0 then return, ''
  
  return,resarr
  
END

; This procedure takes the name of an input manifest and
; read the entries in the file. The task creates a drip object 
; based on the first file and call the run method of the drip
; for each of the files.
; The procedure also takes the name of the output manifest which
; is used to save the name of the products with the format stated 
; in the DCS_ICD
PRO drip_pipe, DMfile, outDMfile=outDMfile, outlist=outlist, debug=debug, path=path
  
  print,'INPUT MANIFEST: '+DMfile
  if keyword_set(outDMfile) then print,'OUTPUT MANIFEST: '+outDMfile
  
  if keyword_set(path) then begin
    DMfile = path+DMfile
    if keyword_set(outDMfile) then outDMfile = path + outDMfile
  endif
  
  if FILE_TEST(DMfile) eq 0 then begin
    print,'Input manifest does not exist: '+DMfile
    return
  endif
  
  if not keyword_set(outlist) then outlist = ['undistorted','merged','coadded']
  outlist = outlist[uniq(outlist)]
  
  
  drip_config_load  
  OPENR, inunit, DMfile, /GET_LUN, /MORE
    
  namepos=strpos(DMfile,path_sep(),/reverse_search)
  if namepos ge 0  then defaultpath = strmid(DMfile,0,namepos+1) $
  else defaultpath = './'
  
  readnum = 0 
  fname = ''
  readf,inunit,readnum
  
  if fix(readnum) le 1 then begin
    k = where(outlist eq 'coadded')
    if k[0] ne -1 then begin
      if n_elements(outlist) eq 1 then begin
        outlist = ['merged']
      endif else begin
        k = where(outlist ne 'coadded')  ; k should be different from -1 since the array contains unique elements and has more than 1 element
	outlist = outlist[k]
      endelse
    endif
  endif
  ; We could potentially check if the outlist contains only valid elements
  ; but I will assume for now that it does since we are managing this function with the sh wrapper.
  
  ; now we create a list that does not contain coadded
  lastoutlist = outlist
  k = where(outlist ne 'coadded')
  if k[0] ne -1 then begin
    if n_elements(outlist) gt 1 then begin
      outlist = lastoutlist[k]
    endif 
  endif else begin
    outlist = ['merged']
  endelse
   
  drip_config_load
    
  readf,inunit,fname
  print,'Reducing file '+fname
  
  ; Check if the initial name has a path
  ; if not we use the default path which is the path of the DM
  pathpos=strpos(fname,path_sep(),/reverse_search)
  if pathpos lt 0  then fname = defaultpath+fname
  
  ; Call run method for the first file
  if not keyword_set(debug) then begin
    ; Create a DRIP object
    drip_obj = drip_new(fname)
    drip_obj->run, fname
    if readnum eq 1 then foutarr = SaveProduct(drip_obj, fname,outlist=lastoutlist) $
    else foutarr = SaveProduct(drip_obj, fname,outlist=outlist)
  endif else begin
    if readnum eq 1 then foutarr = SaveProduct(obj_new(), fname,/nosave,outlist=lastoutlist) $
    else foutarr = SaveProduct(obj_new(), fname,/nosave,outlist=outlist)
  endelse
  
  for i=1,readnum-1 do begin 
    readf,inunit,fname
    
    ; Check if the initial name has a path
    ; if not we use the default path which is the path of the DM
    pathpos=strpos(fname,path_sep(),/reverse_search)
    if pathpos lt 0  then fname = defaultpath+fname
  
    print,'Reducing file '+fname
        
    if not keyword_set(debug) then begin
      drip_obj->run, fname
      if i eq readnum-1 then begin
	if foutarr[0] eq '' then foutarr = SaveProduct(drip_obj, fname,outlist=lastoutlist) $
	else foutarr = SaveProduct(drip_obj, fname, foutarr=foutarr,outlist=lastoutlist)
      endif else begin
	if foutarr[0] eq '' then foutarr = SaveProduct(drip_obj, fname,outlist=outlist) $
	else foutarr = SaveProduct(drip_obj, fname, foutarr=foutarr,outlist=outlist)
      endelse
    endif else begin
      if i eq readnum-1 then begin
	if foutarr[0] eq '' then foutarr = SaveProduct(drip_obj, fname,/nosave,outlist=lastoutlist) $
	else foutarr = SaveProduct(drip_obj, fname, foutarr=foutarr,/nosave,outlist=lastoutlist)
      endif else begin
	if foutarr[0] eq '' then foutarr = SaveProduct(drip_obj, fname,/nosave,outlist=outlist) $
	else foutarr = SaveProduct(drip_obj, fname, foutarr=foutarr,/nosave,outlist=outlist)
      endelse
    endelse
  endfor
  
  CLOSE,inunit
  FREE_LUN,inunit
  
  
  if not keyword_set(outDMfile) then begin
    print,'No output manifest is created'    
    print,'DONE'
    return
  endif
    
  if foutarr[0] eq '' then begin
    OPENW, outunit, outDMfile, /GET_LUN, /MORE
    printf,outunit,'0'
    CLOSE,outunit
    FREE_LUN,outunit
    return
  endif
  
  OPENW, outunit, outDMfile, /GET_LUN, /MORE
  printf,outunit,strtrim(n_elements(foutarr),2)
  for i=0,n_elements(foutarr)-1 do begin
    printf,outunit,foutarr[i]
  endfor  
  CLOSE,outunit
  FREE_LUN,outunit
  
END
