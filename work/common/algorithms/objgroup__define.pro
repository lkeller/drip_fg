; NAME:
;     OBJGROUP - Version .8.0
;
; PURPOSE:
;     Classify data in groups of the same observation
;
; CALLING SEQUENCE:
;     Obj=Obj_new('OBJGROUP', DIRECTORY_PATH)
;
; INPUTS:
;     DIRECTORY_PATH - Path were we want to find the data
;
; STRUCTURE:
;
; OUTPUTS:
;
; CALLED ROUTINES AND OBJECTS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos Llorens, USRA, July, 2010



;****************************************************************************
;     FILL - Create and store the groups
;****************************************************************************
PRO objgroup::fill, skipfilesummary=skipfilesummary
  
  ; Reset ngroups
  self.ngroups = 0
  
  if keyword_set(skipfilesummary) eq 0 then begin
    ; For now we only consider the firts filter
    unsortfiles = findfiles((*(self.filters))[0],root=self.path,/recurse,count=Nfound)

    ; Sort files because findfiles give the files in an arbitrary order
    foundfiles = unsortfiles(sort(unsortfiles))

    ; make groups according to criteria
    ;Nfound = n_elements(foundfiles)
    Nkey = n_elements(*self.criteria)

    if Nfound eq 0 then return

    ; Create an array with a structure containing the filename 
    ; and the (string) value for each keyword
    ; This is to avoid reading the keyword from the header each time
    ; we compare two files
    filesummary = replicate('',Nfound,Nkey+1)
    filesummary(*,0) = foundfiles

    ; These variables is used to keep track of the ditherp number
    ; for two consecutive images in order to make dither groups 
    ; when dgroups criteria is choosen. gdither is an arbitrary number
    ; number that identifies each dither
    previous_ditherp = 100
    numgroup = 1

    ; Read headers and complete filesummary
    for i=0,Nfound-1 do begin
      print,'Checking file '+ filesummary[i,0]
      im = readfits(filesummary[i,0],h,/silent)
      for keycount=0,Nkey-1 do begin
	; I added a case statement to take care of special requirements
	; For example, checking if it is nod-match-chop which requires to compare nod and chop
	; or to check the dither groups. For these, I made up other keyword names that are 
	; not real header keywords
	CASE (*(self.criteria))[keycount] of
          'CHOPMNOD': begin
	    chopdist=float(sxpar(h,'CHPAMP1'))*2. ;2 MCCS sends champ1/2, [was /plate_scale]
	    noddist=float(sxpar(h,'NODAMP'))
	    chopangr=float(sxpar(h,'CHPANGLR'))
	    nodangr=float(sxpar(h,'NODANGLR'))

	    if ((round(noddist) eq round(chopdist)) and (abs(nodangr-chopangr) eq 180)) then $
              filesummary(i,keycount+1) = 'CMN' $
	    else filesummary(i,keycount+1) = 'CPN'	
	  end
	  'DGROUPS':begin
	    dithers = uint(sxpar(h,'DITHERS'))
	    help,dithers
	    ; if the image is not part of a dither group then we assign 0 to the criteria
	    ; if not we check if it is part of the previous group 
	    if dithers eq 1 then begin
	      ; Check if the image is part of the previous group by comparing ditherp number
	      ditherp = sxpar(h,'DITHERP')
	      print,'DITHERP = ',ditherp
	      if ditherp lt previous_ditherp then begin
		; create new numgroup 
		numgroup = numgroup + 1 
	      endif 
	      print,'NGROUP = ',numgroup
	      filesummary(i,keycount+1) = numgroup
	      previous_ditherp = ditherp
	    endif else begin
	      filesummary(i,keycount+1) = 0
	    endelse
	  end
	  else: filesummary(i,keycount+1) = drip_getpar(h,(*(self.criteria))[keycount],/silent)
	ENDCASE
      endfor
    endfor
    *(self.filesummary) = filesummary
  endif else begin
    Nfound = n_elements((*(self.filesummary))[*,0])
    Nkey = n_elements((*(self.filesummary))[0,*])-1
    filesummary = *(self.filesummary)
  endelse
  
  ; Check for each file what are the one that have the same keywords
  ; The filedone array keeps track of the files that have been already grouped
  filedone = replicate(0,Nfound)
  for i=0,Nfound-1 do begin
    if filedone(i) eq 0 then begin
      filedone(i) = 1
      group = ptr_new(/allocate_heap)
      *group = fix([i])
      
      ; check how many files do not allow to a group
      ; files belonging to a group are represented by 
      ; a number one in the filedone array
      kalone = where(filedone eq 0)
      if kalone(0) eq -1 then begin
	; if none
	; Add the last file checked and exit
	if self.ngroups eq 0 then *self.groups = [group] $
	else *self.groups = [*(self.groups),group]
	self.ngroups = self.ngroups+1
	return
      endif else begin
	for filecount=0,n_elements(kalone)-1 do begin
	  compaux = filesummary(i,1:Nkey) eq filesummary(kalone(filecount),1:Nkey)
	  ; Check that all the keywords are the same (that is file1 eq file2 has only 1 values
	  kcomp = where(compaux eq 0)
	  if kcomp(0) eq -1 then begin
	    filedone(kalone(filecount)) = 1
	    *group = fix([*group,kalone(filecount)])
	  endif 
	endfor
      endelse

      if self.ngroups eq 0 then *self.groups = [group] $
      else *self.groups = [*(self.groups),group]
      self.ngroups = self.ngroups+1
    endif
  endfor
  
  ; Here we save the object in the directory we just checked
  ; this will be to avoid rechecking the directory in the future
  objgroup_save={filesummary:*self.filesummary, $
		 filters:*self.filters, $
		 criteria:*self.criteria}
  save,objgroup_save,filename=self.path+'objgroup.sav'
  
END

;****************************************************************************
;     FGROUPS_CLEAN - Because GETDATA has memory leaks when called by an 
;                     external function, this proc allows to clean 
;                     the memory
;****************************************************************************
PRO objgroup::fgroups_clean, fgroups

  for i=0,n_elements(fgroups)-1 do begin
    ptr_free,fgroups[i]
  endfor

END

;****************************************************************************
;     SETDATA - set data values
;****************************************************************************
PRO objgroup::setdata, path=path, criteria=criteria, filters=filters

  if keyword_set(path) then begin
    self.path=path
    self->fill
  endif
  
  if keyword_set(criteria) then begin
    self.criteria=criteria
    self->fill
  endif
  
  if keyword_set(filters) then begin
    self.filters=filters
    self->fill
  endif
  
END

;****************************************************************************
;     GETDATA - get data values
;               WARNING: if getdata is called with fgroups the function
;               creates extra-heaps that remain in memory if they are not 
;               deleted by the task calling getdata. Use fgroups_clean for
;               this purpose
;****************************************************************************

FUNCTION objgroup::getdata, path=path, criteria=criteria, filters=filters,  $
                            ngroups=ngroups, groups=groups, groupi=groupi, $
			    fgroups=fgroups, fgroupi=fgroupi, kgroups=kgroups, $
			    filesummary=filesummary

  if keyword_set(path) then return, self.path
  if keyword_set(filtesr) then return, *self.filters
  if keyword_set(criteria) then return, *self.criteria
  if keyword_set(ngroups) then return, self.ngroups
  if keyword_set(groups) then return, *self.groups
  if keyword_set(groupi) then return, (*self.groups)[groupi]
  
  ; This returns an array of pointers, each pointer 
  ; is the address to an array with the list of filenames of the
  ; same group
  if keyword_set(fgroups) then begin
    if self.ngroups eq 0 then return,null
    
    fgroupaux = ptr_new(/allocate_heap)
    *fgroupaux = (*(self.filesummary))[*((*self.groups)(0)),0]
    resfgroups = [fgroupaux]
    for i=1,self.ngroups-1 do begin
      fgroupaux = ptr_new(/allocate_heap)
      *fgroupaux = (*(self.filesummary))[*((*self.groups)(i)),0]
      resfgroups = [resfgroups,fgroupaux]
    endfor
    return, resfgroups
  endif
  if keyword_set(fgroupi) then begin
    if self.ngroups le fgroupi then return,null
    return, (*(self.filesummary))(*((*self.groups)(fgroupi)),0)
  endif
  if keyword_set(filesummary) then return, *self.filesummary
  
  if keyword_set(kgroups) then begin
    if self.ngroups eq 0 then return,null
    
    ncriteria = n_elements(*self.criteria)
    kgroupaux = replicate('',self.ngroups,ncriteria)
    for i=0,self.ngroups-1 do begin
      kgroupaux[i,*] = (*(self.filesummary))[(*((*self.groups)[i]))[0],1:ncriteria]
    endfor
    return, kgroupaux
  endif
  
  structure={ $
             path:self.path, $
	     criteria:*self.criteria, $
	     filters:*self.filters, $
	     ngroups:self.ngroups, $
	     groups:*self.groups, $
	     filesummary:*self.filesummary $
            }
  
  return,structure
    
END
;****************************************************************************
;     CLEANUP - Destroy pointer heap variables.
;****************************************************************************

PRO objgroup::cleanup
  
  ; cleanup data
  ptr_free, self.criteria
  ; We free the heaps inside self.groups
  for i=0,n_elements(*self.groups)-1 do begin
    ptr_free,(*self.groups)[i]
  endfor  
  ptr_free, self.groups
  ptr_free, self.filters
  ptr_free, self.filesummary

END
;****************************************************************************
;     INIT - Initialize structure fields.
;****************************************************************************

FUNCTION objgroup::init, directory_path, fkeysel=fkeysel, fitler=filter
  
  if N_PARAMS() lt 1 then return,0
  
  ; Allocate memory for pointers  
  self.filesummary = ptr_new(/allocate_heap)
  self.filters=ptr_new(/allocate_heap)
  self.criteria=ptr_new(/allocate_heap) 
  self.groups = ptr_new(/allocate_heap) 
  
  ; store directory path
  self.path = directory_path
  *self.filesummary = -1
  
  ; Initialize values either from inputs/defaults or from sav file
  ; Check if an objgroup.sav file has been previously saved
  ; this means that the directory has been previously checked
  if FILE_TEST(self.path+'objgroup.sav') then begin
    restore,self.path+'objgroup.sav'
    *self.filesummary = objgroup_save.filesummary
    *self.filters     = objgroup_save.filters
    *self.criteria    = objgroup_save.criteria
    
    ; Now create the groups based on filesummary
    self->fill,/skipfilesummary
  endif else begin
    if keyword_set(filter) then *self.filters=filter  $
    else *self.filters=['*.fits']

    ; initialize criteria from dripconf
    if keyword_set(fkeysel) eq 0 then begin
      emptyheader = ['NONE']
      fkeysel_read = drip_getpar(emptyheader,'FKEYSEL')
      if fkeysel_read eq 'x' then return,0  
      *self.criteria=strsplit(fkeysel_read,'[],',/extract)
    endif else begin 
      *self.criteria=fkeysel
    endelse

    ; Fill images based on path
    self->fill
  endelse
  
  return,1
  
END
;****************************************************************************
;     DRIP__DEFINE - Define the DRIP class structure.
;****************************************************************************

PRO objgroup__define

  struct={objgroup, $
          path:'', $                  ; Path were the images are found
	  ngroups:0, $                ; Number of groups created
	  groups:ptr_new(), $          ; Array of arrays with the filenames
	  criteria:ptr_new(), $       ; List of strings of keyword checked for selection
	  filters: ptr_new(), $       ; filters used to select files. Normally '*.fits'
	  filesummary:ptr_new() $     ; 2D Array with filenames and keyword values for each file
	 }

END
