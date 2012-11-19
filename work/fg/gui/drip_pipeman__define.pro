; NAME:
;     DRIP_PIPEMAN - Version 1.7.0
;
; PURPOSE:
;     Pipeline manager for the GUI
;
; DEFINITIONS:
;     PIPE = data reduction PIPEline
;            object for data reduction
;
; CALLING SEQUENCE / INPUTS / OUTPUTS: NA
;
; CALLED ROUTINES AND OBJECTS:
;   USER INTERFACE: PIPEMAN reacts to user input through menus,
;                   GUI buttons and popup dialogs
;   DRIP: Used as pipeline to reduce data
;   CW_DRIP_MW: Used to send messages to the user
;   DRIP_ANAL_SELECT: PIPEMAN updates selected displays when new data
;                     is reduced
;   DRIP_DATAMAN: Is used to store all reduced data
;   DRIP_AUTOMAN: calls PIPEMAN::AUTO_OPEN and ::RUN with new files
;   DRIP_MENU: checks PIPEMAN.SAVEFLAG when exiting
;
; PROCEDURE:
;     New data files are first opened, then reduced (RUN). This can be
;     done by the user throught the GUI or by DRIP_AUTOMAN if the GUI
;     is in automatic reduction mode. All reduced data is stored in
;     DATAMAN, such that it is available to the GUI displays through
;     DRIP_ANAL_SELECT objects. The same objects are notified if new
;     data is available through the CHANNELCALL process of
;     DATAMAN. Various menu items allow configuration of the data
;     reduction pipeline and the selection of the reduction steps to
;     display.
;
; RESTRICTIONS:
;     None
;
; MODIFICATION HISTORY:
;     Written by: Marc Berthoud, Cornell University, July 2006
;     Modified: Marc Berthoud, Cornell University, September 2007
;               - Made responsible of its menu and drip buttons
;               - Added interaction with diplays (auto send reduced
;                 images)
;     Modified: Nirbhik Chitrakar, Cornell Univerisity, August 2006
;               - added saveflag for unsaved reduced data
;     Modified: Luke Keller, Ithaca College, June 2010
;               - Added REGISTER_FRAMES, Pipeline pulldown includes
;                 selection of method to register frames in merge and
;                 coadd
;     Modified: Marc Berthoud, Ithaca College, July 2010
;               - Added popup window for drip pipeline configuration
;               - Removed menu to select frame register method
;               - Added re-reduce (last file) function
;     Modified: Miguel Charcos, USRA, April 2011
;               - Added Saved buttons in the main window (Pipeline Control panel)
;               - Add save configuration
;               - Add autosave
;     Modified: Miguel Charcos, USRA, May 2011
;               - Modify sensitivity of buttons as files are opened and reduced
;               - Add remove files button
;     Modified: Casey Byrne, Ithaca College, July 2012
;               -Added Spectral Extraction Mode Selection to Pipe_Conf widget
;               -Will appear in drip, but will not do anything


;******************************************************************************
;     GETDATA - Returns the SELF structure elements
;******************************************************************************

function drip_pipeman::getdata, saveflag=sf

if keyword_set(sf) then return, self.saveflag

end


;******************************************************************************
;     CHECKSTATEPANEL - Returns the state of the panel as defined in stepipepanel
;                       This function does not detect if there was a step back
;******************************************************************************
FUNCTION drip_pipeman::checksteatepanel
  if self.n eq 0 then return, 0
  if self.ind eq 0 then return, 1
  if self.ind eq self.n then return,3
  
  return,2
END

;******************************************************************************
;     SETPIPEPANEL - Display the appropriate buttons on the pipe panel
;                    0 = No file has been opened or reduced
;                    1 = Files that are not reduced are opened but not previous files reduced 
;                    2 = Files that are not reduced are opened and previous files are reduced
;                    3 = No new files has been opened = there are files and all files are reduced
;******************************************************************************

PRO drip_pipeman::setpipepanel, action, numstat=numstat
		 
  oldnumred = (*self.pipestat).numred
  if keyword_set(numstat) then begin
    possible_stat = [0,1,2,3]
    k = where(possible_stat eq numstat)
    if (k(0) eq -1) then return  
    (*self.pipestat).stat = numstat
  endif else begin
    if keyword_set(action) eq 0 then return
    CASE action of
      'same' : ; no changes on the stat. We update the widgets according to the current state
      'state': begin
               ; check what is the status based on the number of files opened and reduce
               ; I believe that we could just use this and skip the reset, open, remove and reduce
	       ; but I leave for now because they work
               (*self.pipestat).stat = self->checksteatepanel()
               (*self.pipestat).numred = self.n-self.ind
	     end
      'reset': begin
               (*self.pipestat).stat = 0
               (*self.pipestat).numred = 0
	     end
      'open' : begin	       
	       if (*self.pipestat).stat eq 0 then (*self.pipestat).stat = 1 $
	       else if (*self.pipestat).stat eq 1 then (*self.pipestat).stat = 1 $
	       else if (*self.pipestat).stat eq 2 then (*self.pipestat).stat = 2 $
	       else if (*self.pipestat).stat eq 3 then (*self.pipestat).stat = 2 
	     end
      'remove' : begin               
		 if (*self.pipestat).stat eq 0 then return  $
		 else if (*self.pipestat).stat eq 1 then (*self.pipestat).stat = 0 $
		 else if (*self.pipestat).stat eq 2 then (*self.pipestat).stat = 3 $
		 else if (*self.pipestat).stat eq 3 then return
	     end
      'reduce' : begin
		 if (*self.pipestat).stat eq 0 then return $
		   ;else if (*self.pipestat).stat eq 3 then return $
		 else if (*self.pipestat).stat eq 1 then (*self.pipestat).stat = 3 $
		 else if (*self.pipestat).stat eq 2 then (*self.pipestat).stat = 3 
		 (*self.pipestat).numred = (*self.pipestat).numred + 1
		 (*self.pipestat).didstepback = 0
	     end
      'stepback' : begin 
		 if (*self.pipestat).numred eq 0 then return
		 
		 if (*self.pipestat).stat eq 0 or (*self.pipestat).stat eq 1 then return 
		 if (*self.pipestat).stat eq 2 then begin
		   if (*self.pipestat).numred eq 1 then (*self.pipestat).stat = 1 
		 endif else begin  ; (*self.pipestat).stat eq 3
		   if (*self.pipestat).numred eq 1 then (*self.pipestat).stat = 0 
		 endelse		 		 
		 (*self.pipestat).numred = (*self.pipestat).numred - 1
		 (*self.pipestat).didstepback = 1
	     end
      'none': begin
		widget_control,(*self.pipestat).resetpipe, sensitive=0
		widget_control,(*self.pipestat).removefile, sensitive=0
		widget_control,(*self.pipestat).reduce, sensitive=0
		widget_control,(*self.pipestat).stepback, sensitive=0
		widget_control,(*self.pipestat).open, sensitive=0
        	return
	      end
      'all': begin
		; I do not think we need this case. But it does not cost much
		; to implement it
		widget_control,(*self.pipestat).resetpipe, sensitive=1
		widget_control,(*self.pipestat).removefile, sensitive=1
		widget_control,(*self.pipestat).reduce, sensitive=1
		widget_control,(*self.pipestat).stepback, sensitive=1
		widget_control,(*self.pipestat).open, sensitive=1
        	return
	      end
      else: return
    ENDCASE
  endelse
  
  ; For any state the open button is available. In fact we can always load data
  ; The only scenario in which we can't is when auto-reducing
  ; This scenario is treated before using none (no button should be available during 
  ; auto-reduce). In that case, the procedure has already returned
  widget_control,(*self.pipestat).open, sensitive=1
  CASE (*self.pipestat).stat of
    0: begin 
       widget_control,(*self.pipestat).resetpipe, sensitive=0
       widget_control,(*self.pipestat).removefile, sensitive=0
       widget_control,(*self.pipestat).reduce, sensitive=0
       widget_control,(*self.pipestat).stepback, sensitive=0
       widget_control,(*self.pipestat).stepbacklist, sensitive=0
     end
    1: begin 
       widget_control,(*self.pipestat).resetpipe, sensitive=1
       widget_control,(*self.pipestat).removefile, sensitive=1
       widget_control,(*self.pipestat).reduce, sensitive=1
       widget_control,(*self.pipestat).stepback, sensitive=0
       widget_control,(*self.pipestat).stepbacklist, sensitive=0
     end
    2: begin 
       widget_control,(*self.pipestat).resetpipe, sensitive=1
       widget_control,(*self.pipestat).removefile, sensitive=1
       widget_control,(*self.pipestat).reduce, sensitive=1
       if ((*self.pipestat).didstepback eq 1) then $
	 widget_control,(*self.pipestat).stepback, sensitive=0  $
       else $
	 widget_control,(*self.pipestat).stepback, sensitive=1
	 
       if self.drip->getdata(/n) eq self.drip->getdata(/lastlistN) then $
         widget_control,(*self.pipestat).stepbacklist, sensitive=0 $
       else $
         widget_control,(*self.pipestat).stepbacklist, sensitive=1
     end
    3: begin 
       widget_control,(*self.pipestat).resetpipe, sensitive=1
       widget_control,(*self.pipestat).removefile, sensitive=0
       widget_control,(*self.pipestat).reduce, sensitive=0
       if ((*self.pipestat).didstepback eq 1) then $
	 widget_control,(*self.pipestat).stepback, sensitive=0  $
       else $
	 widget_control,(*self.pipestat).stepback, sensitive=1
	 
       if self.drip->getdata(/n) eq self.drip->getdata(/lastlistN) then $
         widget_control,(*self.pipestat).stepbacklist, sensitive=0 $
       else $
         widget_control,(*self.pipestat).stepbacklist, sensitive=1
     end
    else:
  ENDCASE
  widget_control,(*self.pipestat).stepbacklist, sensitive=0
END

;******************************************************************************
;     TEST - Used to test when developping.  Remove later
;******************************************************************************

pro drip_pipeman::test, event
  
  a = self.dataman
 ; help,a.daps(4)
 ; help,*a.daps(0)
 ; help,*a.daps(3)
  
end

;******************************************************************************
;     RESET - Reset manager (also pipe count)
;******************************************************************************

pro drip_pipeman::reset
; reset pipeline
obj_destroy, self.drip  ;new pipeline
*self.filelist=['']
self.n=0    ;clear var's
self.ind=0
self.mode=''
self.cnt=0
self.name=''
self.next=0
end

;******************************************************************************
;     NEW - Initialize new pipe
;******************************************************************************
pro drip_pipeman::new, event
obj_destroy, self.drip  ;new pipeline
*self.filelist=['']
self.n=0    ;clear var's
self.ind=0
self.mode=''
self.cnt=0

self.mw->print, '-----------------------'
self.mw->print, '--- NEW EMPTY PIPE ----'
self.mw->print, '-----------------------'
self->setpipepanel,'reset'

end

;******************************************************************************
;     AUTO_OPEN - handle files picked automatically
;******************************************************************************

pro drip_pipeman::auto_open, filelist

if (filelist[0] ne '') then begin
    ; make new drip object with file list, initialize file list
    if self.n eq 0 then begin
        drip=drip_new(filelist) ;create drip object
        if drip ne obj_new() then begin
            ; store new data in local variables
            self.drip=drip
            *self.filelist=filelist
            self.n=n_elements(filelist)
            self.mode=drip->getdata(/mode)
            ; make name for new pipeline
            fits_read, filelist[0], null, header, /header
            detector=strtrim(sxpar(header,'DETECTOR'),2)
            self.name='FC_'+self.mode+'_'+detector+'_'+ $
                      strtrim(string(self.next),2)
            self.next=self.next+1
	    
	    ; update saveconfstat
	    ; list of calibration products
	    ncal = n_elements((*((self.drip).callist)))
	    *((*(self.saveconfstat)).callist) = strarr(ncal)
	    *((*(self.saveconfstat)).calsel) = lonarr(ncal)
	    *((*(self.saveconfstat)).calval) = replicate(1,ncal)
	    if self.savecalpath eq '' then self.savecalpath = (self.drip)->getdata(/pathcalsave) $
	    else self.drip->setdata,pathcalsave=self.savecalpath
	    (*(self.saveconfstat)).calmakecube = 1
	    for i=0,ncal-1 do begin
	      (*((*(self.saveconfstat)).callist))[i] = (*(drip.callist))[i].name
	    endfor
	    nred = n_elements((*((self.drip).reducedlist)))
	    *((*(self.saveconfstat)).reducedlist) = strarr(nred)
	    *((*(self.saveconfstat)).redsel) = lonarr(nred)
	    *((*(self.saveconfstat)).redval) = replicate(1,nred)
	    if self.savepath eq '' then self.savepath = (self.drip)->getdata(/pathsave) $
	    else self.drip->setdata,pathsave=self.savepath
	    (*(self.saveconfstat)).redmakecube = 1
	    for i=0,nred-1 do begin
	      (*(*(self.saveconfstat)).reducedlist)[i] = (*(drip.reducedlist))[i].name
	    endfor
	    
	    (*(self.saveconfstat)).makecube = 0
        endif
    ; else (filelist exists) simply add to it
    endif else begin
        file=*self.filelist
        file=[file, filelist]
        *self.filelist=file
        self.n=n_elements(file)
    endelse
    ; print message
    if self.n gt 0 then begin
        sz=size(filelist, /n_elements)
        if sz gt 1 then self.mw->print, 'files ' + $
          file_basename(filelist[0]) + ' to ' + $
          file_basename(filelist[sz-1]) + ' added to filelist' else $
          self.mw->print, 'file ' + file_basename(filelist) + ' added to filelist'
	  self->setpipepanel,'open'
    endif else begin
        self.mw->print, 'ERROR: auto_open could not initialize pipe from ' + $
          file_basename(filelist[0])
    endelse
endif
end

;******************************************************************************
;     OPENGROUP - pick directory and organize groups for reduction
;******************************************************************************

PRO drip_pipeman::opengroup, event, allfiles=allfiles

  common gui_config_info, guiconf
  
  eventid = event.id
  if (self.objgroup ne OBJ_NEW()) then begin
    if event.id eq (*self.pipestat).open OR $
       event.id eq (*self.pipestat).openmenu then eventid = 0
  endif else begin
    eventid = (*self.pipestat).open
  endelse
  ; if button is selectnewdirectory then selectdir=0
  
  ; This is the default maximum number of files we display per column 
  ; We can define this in dripconf but for now I choose this which looks good enough
  nfilescolumn = 15
  inallpush = fix(getpar(guiconf,'allgrouppush'))
  
  switch eventid of
    (*self.pipestat).openmenu:
    (*self.pipestat).open: begin
	;let user choose a directory
	rootdir=dialog_pickfile( filter=*self.loadfilter,/fix_filter, $
        			/must_exist, /read, get_path=path, $
	  			path=self.loaddatapath, dialog_parent=event.top, DIRECTORY=1)

	; Return if the user cancelled the operation
	if rootdir eq '' then return

	; Create groups only if we changed path
	; Selecting groups can be a long process and we
	; do not want to start if it is not required
	if path ne self.loaddatapath OR self.objgroup eq OBJ_NEW() then begin
	  if self.objgroup ne OBJ_NEW() then obj_destroy,self.objgroup
          self.objgroup = OBJ_NEW('OBJGROUP',rootdir)
	endif  
	
	ngroups = self.objgroup->getdata(/ngroups)
	if ngroups eq 0 then begin
	  obj_destroy,self.objgroup
	  self.objgroup=OBJ_NEW()
	  break
	endif
	; store path and loadfilter
	self.loaddatapath=path ;store path
	
	; Call opengroup with eventid=0 for selecting the group in that directory
	self->opengroup,{id:0, top:1, handler:0, select:1}
	break
    end
    ; for selecting file group - open selection window
    0:begin
        ; Check if window is closed (i.e. stat=0) and dispn > 0
        if ((*self.groupconf).stat eq 0) then begin
            ; make label and text widgets

            top=widget_base(/column);, xoffset=(*self.groupconf).topxpos, yoffset=(*self.groupconf).topypos)

	    (*self.groupconf).top = top
            label=widget_label(top,font=largefont,value='SELECT GROUP OF FILES')
            info1=widget_label(top,value='Select what products you want to load ')
	    
	    ; Create list of files	       
	    (*self.groupconf).filebase = widget_base(top,/row)
	    ; Create columns of the table
	    groupcriteria = self.objgroup->getdata(/criteria)
	    framegroup = widget_base((*self.groupconf).filebase,/column)
	    criteria = self.objgroup->getdata(/criteria)
	    ncriteria = n_elements(criteria)
	    ngroups = self.objgroup->getdata(/ngroups)
	    
	    ; exit if no groups
	    if ngroups eq 0 then return
	    
	    if ngroups gt nfilescolumn then $
	      tabcolumn = widget_base(framegroup,/row,/frame,/scroll,X_SCROLL_SIZE=900,Y_SCROLL_SIZE=500) $
	    else $
	      tabcolumn = widget_base(framegroup,/row,/frame)	    
	    criteria_column = replicate(0L,ncriteria+2)
	    criteria_column0=widget_base(tabcolumn,/column)
	    criteria_label = widget_label(criteria_column0,value='#')
	    criteria_column[0]=widget_base(criteria_column0,/column,/Exclusive)
	    for i=0,ncriteria-1 do begin
	      criteria_column[i+1]=widget_base(tabcolumn,/column)
	      criteria_label = widget_label(criteria_column[i+1],value=groupcriteria[i])
	    endfor
	    criteria_column[ncriteria+1]=widget_base(tabcolumn,/column)
	    criteria_label = widget_label(criteria_column[ncriteria+1],value='NFILES')
	    	      
	    ; Creates the list of groups
	    (*((*self.groupconf).grouplist)) = lonarr(ngroups)
	    kgroups = self.objgroup->getdata(/kgroups)
	    fgroups = self.objgroup->getdata(/fgroups)
	    ;groupbase = widget_base((*self.groupconf).filebase,/column,/Exclusive)
	    for i=0,ngroups-1 do begin
	      (*((*self.groupconf).grouplist))[i] = widget_button(criteria_column[0], $
                  value=strtrim(i,1), $  ;filesummary(0,i)
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'opengroup'} )
	      for countkey=0,ncriteria-1 do begin
	        labelkey = widget_label(criteria_column[countkey+1],value=kgroups(i,countkey),ysize=25)
	      endfor
	      ; Add info in column dedicated to the number of files
	      auxnfiles = n_elements(*(fgroups[i]))
	      labelkey = widget_label(criteria_column[ncriteria+1],value=strtrim(auxnfiles,1),ysize=25)
	    endfor
	    widget_control,(*((*self.groupconf).grouplist))[(*self.groupconf).groupsel],/set_button
	    	    
	    ; Create the list of files for the selected group
            (*self.groupconf).listbase=widget_base((*self.groupconf).filebase,/column,/scroll,X_SCROLL_SIZE=300,Y_SCROLL_SIZE=500)
	    nfiles = n_elements(*(fgroups[(*self.groupconf).groupsel]))
	    	        	    
	    filelabel = widget_label((*self.groupconf).listbase,value=strtrim(nfiles,1)+' FILES in GROUP '+strtrim((*self.groupconf).groupsel,1),/frame)
	    (*((*self.groupconf).filelist)) = lonarr(nfiles)	
	    fbasemain = widget_base((*self.groupconf).listbase,/row)
	    fsubbase = widget_base(fbasemain,/column,/NonExclusive)
	    trackcolumn = 0
	    *((*self.groupconf).fileval) = replicate(1,nfiles)
	    for i=0,nfiles-1 do begin
	      filename = (*(fgroups[(*self.groupconf).groupsel]))[i]
	      namepos=strpos(filename,path_sep(),/reverse_search)
	      fname = strmid(filename,namepos+1,strlen(filename))
	      pname = strmid(filename,0,namepos)
	      (*((*self.groupconf).filelist))[i] = widget_button(fsubbase, $
                  value=fname, $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'opengroup'}, $
		  Tooltip=pname)
	      if inallpush eq 1 then begin
		widget_control,(*((*self.groupconf).filelist))[i],/set_button
	      endif else begin
		widget_control,(*((*self.groupconf).filelist))[i],set_button=0
		(*((*self.groupconf).fileval))[i] = 0
	      endelse
	      ;if i gt trackcolumn +nfilescolumn then begin
	      ;  fsubbase = widget_base(fbasemain,/column,/NonExclusive)
	      ;	trackcolumn = trackcolumn + nfilescolumn
	      ;endif
	    endfor
	    	    
            ; make button widgets
	    bottombase = widget_base(top,/row)
	    butbase = widget_base(bottombase,/column)
	    newbase = widget_base(butbase,/row)
	    (*self.groupconf).newbut = widget_button(newbase, $
                  value='Change Directory', $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'opengroup'} )
	    (*self.groupconf).refreshbut = widget_button(newbase, $
                  value='Refresh Groups', $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'opengroup'} )
		  
            (*self.groupconf).dosetbut=widget_button(butbase, value='Basic Science Set', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'opengroup'} )
            (*self.groupconf).doallbut=widget_button(butbase, value='Basic Science All', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'opengroup'} )
            row=widget_base(butbase,/row)
            (*self.groupconf).done=widget_button(row, value='Open Files', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'opengroup'} )
            (*self.groupconf).cancel=widget_button(row, value='Close Window', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'opengroup'} )
            ; set status
            (*self.groupconf).stat=1
	    
	    ; Make Saving widget
	    (*self.groupconf).savebase = widget_base(bottombase,/column)
	    
            ; realize and start widgets

            widget_control, top, /realize, xoffset=300, yoffset=0;, xoffset=(*self.groupconf).topxpos, yoffset=(*self.groupconf).topypos

            xmanager, 'Select and Open Files', top
	    
	    ; Clean memory - remove fgroups heaps
	    self.objgroup->fgroups_clean,fgroups
        endif
        break
    end
    ; Change directory
    (*self.groupconf).newbut:begin
	;let user choose a directory
	rootdir=dialog_pickfile( filter=*self.loadfilter,/fix_filter, $
        			/must_exist, /read, get_path=path, $
	  			path=self.loaddatapath, dialog_parent=event.top, DIRECTORY=1)

	; Return if the user cancelled the operation
	if rootdir eq '' then return

	; Create groups a new group
	if path ne self.loaddatapath then begin
	  ; Remove the current window
	  if (*self.groupconf).stat ne 0 then begin
              widget_control, event.top, /destroy
              (*self.groupconf).stat=0
          endif
          (*self.groupconf).groupsel = 0
	  
	  if self.objgroup ne OBJ_NEW() then OBJ_DESTROY,self.objgroup
	  self.objgroup = OBJ_NEW('OBJGROUP',rootdir)
          
	  ; store path and loadfilter
	  self.loaddatapath=path ;store path

	  ; Call opengroup with eventid=0 for opening a new window 
	  ; for the new directory
	  self->opengroup,{id:0, top:1, handler:0, select:1}
	endif
	break
    end
    ; Refresh groups
    (*self.groupconf).refreshbut:begin
	; Remove the current window
	if (*self.groupconf).stat ne 0 then begin
            widget_control, event.top, /destroy
            (*self.groupconf).stat=0
        endif
        
	
	; Here we should just fill the object again instead of creating a new one
	;if self.objgroup ne OBJ_NEW() then OBJ_DESTROY,self.objgroup
	;self.objgroup = OBJ_NEW('OBJGROUP',self.loaddatapath)
        
	; Fill the object
	self.objgroup->fill
	ngroups = self.objgroup->getdata(/ngroups)
	
	if ngroups le (*self.groupconf).groupsel then $
	  (*self.groupconf).groupsel = 0
	
	; Call opengroup with eventid=0 for opening a new window 
	; for the new directory
	self->opengroup,{id:0, top:1, handler:0, select:1}
	break
    end    
    ; done: close window and more
    (*self.groupconf).done:begin
	if (*self.groupconf).stat ne 0 then begin
            widget_control, event.top, /destroy
            (*self.groupconf).stat=0
        endif
	; Now we load the selected group in the drip
	ngroups = self.objgroup->getdata(/ngroups)
	if (*self.groupconf).groupsel ge 0 and $
	   (*self.groupconf).groupsel lt ngroups then begin

	  fgroups = self.objgroup->getdata(/fgroups)
	  currentgroup = *(fgroups[(*self.groupconf).groupsel])
	  nfiles = n_elements(currentgroup)
	  
	  ; Make a list with the files that have been selected in the group
	  filelist=['']
	  for i=0,nfiles-1 do begin
	    if (*((*self.groupconf).fileval))[i] eq 1 then $ 
	      filelist=[filelist,(*(fgroups[(*self.groupconf).groupsel]))[i]]
	  endfor
	  
	  nselect = n_elements(filelist)
	  ; If files were selected then open files in the pipeline
	  if nselect gt 1 then $
            self->open, {id:0, top:1, handler:0, select:1}, filelist=filelist[1:nselect-1]
	  ; Open selected group
	  ; We may have to reset the pipeline
	  ; we can add a selection box to let the user choose this option
	  ; self->open,{id:0, top:1, handler:0, select:1},filelist=
	  
	  
	  ; Clean memory - remove fgroups heaps
	  self.objgroup->fgroups_clean,fgroups
	endif
        break
    end    
    ; savego: Save the selected products on the check box
    (*self.groupconf).savego:begin
    
	; Record the x and y offsets of the windows
	auxgeometry = widget_info((*self.groupconf).top,/GEOMETRY)
	(*self.groupconf).topxpos = auxgeometry.xoffset
	(*self.groupconf).topypos = auxgeometry.yoffset
	
	ngroups = self.objgroup->getdata(/ngroups)
	if (*self.groupconf).groupsel ge 0 and $
	   (*self.groupconf).groupsel lt ngroups then begin
          
	  fgroups = self.objgroup->getdata(/fgroups)
	  currentgroup = *(fgroups[(*self.groupconf).groupsel])	  
	  ; Clean memory - remove fgroups heaps
	  self.objgroup->fgroups_clean,fgroups
	  
	  nfiles = n_elements(currentgroup)
	  
	  if keyword_set(allfiles) then begin
	    nselect = nfiles
	    filelist = currentgroup
	  endif else begin
	    ; Make a list with the files that have been selected in the group
	    filelist=['']
	    for i=0,nfiles-1 do begin
	      if (*((*self.groupconf).fileval))[i] eq 1 then $ 
		filelist=[filelist,currentgroup[i]]
	    endfor	  
	    nselect = n_elements(filelist)
	    if nselect gt 1 then filelist = filelist[1:nselect-1]
	    nselect = nselect - 1
	  endelse	  
	  
	  if nselect gt 0 then begin
	    ; Remove all the coadds except the last one
	    for i=0,nselect-2 do begin
	      filename = filelist[i]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	     ; print, 'Remove file '+rootname+'_coadded.fits'
	      FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET
	    endfor

	    ; Remove all undistorted if first checkbox is not selected
	    if (*self.groupconf).saveval[0] eq 0 then begin
	      for i=0,nselect-1 do begin
		filename = filelist[i]
		namepos=strpos(filename,'.',/reverse_search)
		rootname = strmid(filename,0,namepos)
		namepos=strpos(rootname,path_sep(),/reverse_search)
		rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
		;print, 'Remove file '+rootname+'_undistorted.fits'
		FILE_DELETE, rootname+'_undistorted.fits', /ALLOW_NONEXISTENT, /QUIET
	      endfor	  
	    endif

	    ; Remove all merged if second checkbox is not selected
	    if (*self.groupconf).saveval[1] eq 0 then begin
	      for i=0,nselect-1 do begin
		filename = filelist[i]
		namepos=strpos(filename,'.',/reverse_search)
		rootname = strmid(filename,0,namepos)
		namepos=strpos(rootname,path_sep(),/reverse_search)
		rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
		;print, 'Remove file '+rootname+'_merged.fits'
		FILE_DELETE, rootname+'_merged.fits', /ALLOW_NONEXISTENT, /QUIET
	      endfor    
	    endif

	    ; Remove last coadded if third checkbox is not selected
	    if (*self.groupconf).saveval[2] eq 0 then begin
	      filename = filelist[nselect-1]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	      ;print,'Remove file '+rootname+'_coadded.fits'
	      FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET	  
	    endif
	  endif
	  widget_control,(*self.groupconf).refreshbut, sensitive=1
          widget_control,(*self.groupconf).newbut, sensitive=1
	  widget_control,(*self.groupconf).done, sensitive=1
	  widget_control,(*self.groupconf).cancel, sensitive=1
          widget_control,(*self.groupconf).doallbut, sensitive=1
          widget_control,(*self.groupconf).dosetbut, sensitive=1
	  widget_control,(*self.groupconf).filebase,sensitive=1
	  
	  widget_control,(*self.groupconf).savecleanbase,/destroy
	  widget_control,(*self.groupconf).top, $

	  xoffset=300, yoffset=0;xoffset=(*self.groupconf).topxpos, yoffset=(*self.groupconf).topypos

	endif
	break
    end
    ; savecancel: Cancel saving the selected products on the check box
    (*self.groupconf).savecancel:begin
    
	; Record the x and y offsets of the windows
	auxgeometry = widget_info((*self.groupconf).top,/GEOMETRY)
	(*self.groupconf).topxpos = auxgeometry.xoffset
	(*self.groupconf).topypos = auxgeometry.yoffset
	      
	ngroups = self.objgroup->getdata(/ngroups)
	if (*self.groupconf).groupsel ge 0 and $
	   (*self.groupconf).groupsel lt ngroups then begin

	  fgroups = self.objgroup->getdata(/fgroups)
	  currentgroup = *(fgroups[(*self.groupconf).groupsel])
	  ; Clean memory - remove fgroups heaps
	  self.objgroup->fgroups_clean,fgroups
	  nfiles = n_elements(currentgroup)
	  
	  if keyword_set(allfiles) then begin
	    nselect = nfiles
	    filelist = currentgroup
	  endif else begin
	    ; Make a list with the files that have been selected in the group
	    filelist=['']
	    for i=0,nfiles-1 do begin
	      if (*((*self.groupconf).fileval))[i] eq 1 then $ 
		filelist=[filelist,currentgroup[i]]
	    endfor	  
	    nselect = n_elements(filelist)
	    if nselect gt 1 then filelist = filelist[1:nselect-1]
	    nselect = nselect - 1
	  endelse
	  
	  if nselect gt 0 then begin
	    ; Remove all the cubes
	    for i=0,nselect-1 do begin
	      filename = filelist[i]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	      ;print, 'Remove file '+rootname+'_allred.fits'
	      FILE_DELETE, rootname+'_allred.fits', /ALLOW_NONEXISTENT, /QUIET
	    endfor
	    ; Remove all the cal cubes
	    for i=0,nselect-1 do begin
	      filename = filelist[i]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savecalpath + strmid(rootname,namepos+1,strlen(rootname))
	     ; print, 'Remove file '+rootname+'_allcal.fits'
	      FILE_DELETE, rootname+'_allcal.fits', /ALLOW_NONEXISTENT, /QUIET
	    endfor
	    ; Remove all the undistorted
	    for i=0,nselect-1 do begin
	      filename = filelist[i]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	     ; print, 'Remove file '+rootname+'_undistorted.fits'
	      FILE_DELETE, rootname+'_undistorted.fits', /ALLOW_NONEXISTENT, /QUIET
	    endfor
	    ; Remove all the merged
	    for i=0,nselect-1 do begin
	      filename = filelist[i]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	     ; print, 'Remove file '+rootname+'_merged.fits'
	      FILE_DELETE, rootname+'_merged.fits', /ALLOW_NONEXISTENT, /QUIET
	    endfor
	    ; Remove all the coadded	      
	    for i=0,nselect-1 do begin
	      filename = filelist[i]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	      ;print, 'Remove file '+rootname+'_coadded.fits'
	      FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET
	    endfor
	  endif
	  widget_control,(*self.groupconf).refreshbut, sensitive=1
          widget_control,(*self.groupconf).newbut, sensitive=1
	  widget_control,(*self.groupconf).done, sensitive=1
	  widget_control,(*self.groupconf).cancel, sensitive=1
          widget_control,(*self.groupconf).doallbut, sensitive=1
          widget_control,(*self.groupconf).dosetbut, sensitive=1
	  widget_control,(*self.groupconf).filebase,sensitive=1
	  
	  widget_control,(*self.groupconf).savecleanbase,/destroy
	  widget_control,(*self.groupconf).top, $

	  xoffset=300, yoffset=0;xoffset=(*self.groupconf).topxpos, yoffset=0;(*self.groupconf).topypos

	endif 
	break
    end
    ; done: close window and reduce this set
    (*self.groupconf).dosetbut:begin
	;if (*self.groupconf).stat ne 0 then begin
        ;    widget_control, event.top, /destroy
        ;    (*self.groupconf).stat=0
        ;endif
	; Now we load the selected group in the drip
	
	; Record the x and y offsets of the windows
	auxgeometry = widget_info((*self.groupconf).top,/GEOMETRY)
	(*self.groupconf).topxpos = auxgeometry.xoffset
	(*self.groupconf).topypos = auxgeometry.yoffset
	
	ngroups = self.objgroup->getdata(/ngroups)
	if (*self.groupconf).groupsel ge 0 and $
	   (*self.groupconf).groupsel lt ngroups then begin

	  fgroups = self.objgroup->getdata(/fgroups)
	  currentgroup = *(fgroups[(*self.groupconf).groupsel])
	  nfiles = n_elements(currentgroup)
	  
	  
	  ; Make a list with the files that have been selected in the group
	  filelist=['']
	  for i=0,nfiles-1 do begin
	    if (*((*self.groupconf).fileval))[i] eq 1 then $ 
	      filelist=[filelist,(*(fgroups[(*self.groupconf).groupsel]))[i]]
	  endfor
	  ; Clean memory - remove fgroups heaps
	  self.objgroup->fgroups_clean,fgroups
	  nselect = n_elements(filelist)
	  
	  ; If files were selected then open files in the pipeline
	  if nselect gt 1 then begin
	    ; Update bottom right panel to indicate the info of the set that we are processing
	    (*self.groupconf).savecleanbase = widget_base((*self.groupconf).savebase,/column)
	    auxmsglab = widget_label((*self.groupconf).savecleanbase,value='Processing group #'+strtrim((*self.groupconf).groupsel,1))
	    widget_control,(*self.groupconf).refreshbut, sensitive=0
            widget_control,(*self.groupconf).newbut, sensitive=0
	    widget_control,(*self.groupconf).done, sensitive=0
	    ;widget_control,(*self.groupconf).cancel, sensitive=0
            widget_control,(*self.groupconf).doallbut, sensitive=0
            widget_control,(*self.groupconf).dosetbut, sensitive=0
	    widget_control,(*self.groupconf).filebase,sensitive=0

	    ; Reposition top window
	    widget_control,(*self.groupconf).top, $

	    xoffset=300, yoffset=0;xoffset=(*self.groupconf).topxpos, yoffset=0;(*self.groupconf).topypos

	    ;self->new,{id:0, top:1, handler:0, select:1}
            self->reset
	    self->open, {id:0, top:1, handler:0, select:1}, filelist=filelist[1:nselect-1]
	    self->run, {id:0, top:1, handler:0, select:1}, /BASIC_SCIENCE
	    
	    msg = ["Choose what products you want to keep. If CANCEL, DRIP will remove", $
	           "all created products including data and calibration cubes"]
	    
	    
	    ; Record the x and y offsets of the windows
	    auxgeometry = widget_info((*self.groupconf).top,/GEOMETRY)
	    (*self.groupconf).topxpos = auxgeometry.xoffset
	    (*self.groupconf).topypos = auxgeometry.yoffset
	    
	    ; Create message to select data
	    widget_control, (*self.groupconf).savecleanbase, /destroy
	    (*self.groupconf).savecleanbase = widget_base((*self.groupconf).savebase,/column)
	    for i = 0,n_elements(msg)-1 do begin
	      auxmsglab = widget_label((*self.groupconf).savecleanbase,value=msg(i))
	    endfor
	    savetypes = widget_base((*self.groupconf).savecleanbase,/row)
	    boxsel = widget_base(savetypes,/NonExclusive,/column)
	    (*self.groupconf).savelist[0] = widget_button(boxsel, value='Undistorted', $
        						  event_pro='drip_eventhand', $
        						  uvalue={object:self, method:'opengroup'} )
	    if (*self.groupconf).saveval[0] eq 1 then $
	      widget_control,(*self.groupconf).savelist[0],/set_button
	    (*self.groupconf).savelist[1] = widget_button(boxsel, value='Merded', $
        						  event_pro='drip_eventhand', $
        						  uvalue={object:self, method:'opengroup'} )
	    if self.mode eq 'C2NC2' then begin
	      widget_control,(*self.groupconf).savelist[1],sensitive=0
	    endif else begin
	      if (*self.groupconf).saveval[1] eq 1 then $
		widget_control,(*self.groupconf).savelist[1],/set_button
	    endelse
	    
	    (*self.groupconf).savelist[2] = widget_button(boxsel, value='Coadded', $
        						  event_pro='drip_eventhand', $
        						  uvalue={object:self, method:'opengroup'} )
	    if (*self.groupconf).saveval[2] eq 1 then $
	      widget_control,(*self.groupconf).savelist[2],/set_button
	      
	      
	    butsel = widget_base(savetypes,/column)
	    (*self.groupconf).savego = widget_button(butsel,value='Save Selected Products', $
        						  event_pro='drip_eventhand', $
        						  uvalue={object:self, method:'opengroup'} )
	    (*self.groupconf).savecancel = widget_button(butsel,value='Cancel', $
        						  event_pro='drip_eventhand', $
        						  uvalue={object:self, method:'opengroup'} )
	    
	    widget_control,(*self.groupconf).top, $

	    xoffset=300, yoffset=0;xoffset=(*self.groupconf).topxpos, yoffset=0;(*self.groupconf).topypos

;	    break

	    ; Remove all the coadds except the last one
;	    for i=0,nfiles-2 do begin
;	      filename = currentgroup[i]
;	      namepos=strpos(filename,'.',/reverse_search)
;	      rootname = strmid(filename,0,namepos)
;	      namepos=strpos(rootname,path_sep(),/reverse_search)
;	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
;	      print, 'Remove file '+rootname+'_coadded.fits'
;	      FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET
;	    endfor

;	    if useful eq 'Yes' then begin
	      ; Here we delete all the undistort corresponding to these files 
	      ; And the coadds except for the last one
;	      for i=0,nfiles-1 do begin
;		filename = currentgroup[i]
;		namepos=strpos(filename,'.',/reverse_search)
;		rootname = strmid(filename,0,namepos)
;		namepos=strpos(rootname,path_sep(),/reverse_search)
;		rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
;		print, 'Remove file '+rootname+'_undistorted.fits'
;		FILE_DELETE, rootname+'_undistorted.fits', /ALLOW_NONEXISTENT, /QUIET
;	      endfor
;	      if nfiles eq 1 then begin
;		print,'Remove file '+rootname+'_coadded.fits'
;		FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET
;	      endif
;	    endif else begin ; useful eq 'No'
;	      if useful eq 'No' then begin
;		; Here we delete all the merge and the coadds
;		for i=0,nfiles-1 do begin
;		  filename = currentgroup[i]
;		  namepos=strpos(filename,'.',/reverse_search)
;		  rootname = strmid(filename,0,namepos)
;		  namepos=strpos(rootname,path_sep(),/reverse_search)
;		  rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
;		  print, 'Remove file '+rootname+'_merged.fits'
;		  FILE_DELETE, rootname+'_merged.fits', /ALLOW_NONEXISTENT, /QUIET
;		endfor
;		filename = currentgroup[nfiles-1]
;		namepos=strpos(filename,'.',/reverse_search)
;		rootname = strmid(filename,0,namepos)
;		namepos=strpos(rootname,path_sep(),/reverse_search)
;		rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
;		print,'Remove file '+rootname+'_coadded.fits'
;		FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET
;	      endif else begin  ; We push the Cancel button
;		; Remove all the undistorted
;		for i=0,nfiles-1 do begin
;		  filename = currentgroup[i]
;		  namepos=strpos(filename,'.',/reverse_search)
;		  rootname = strmid(filename,0,namepos)
;		  namepos=strpos(rootname,path_sep(),/reverse_search)
;		  rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
;		  print, 'Remove file '+rootname+'_undistorted.fits'
;		  FILE_DELETE, rootname+'_undistorted.fits', /ALLOW_NONEXISTENT, /QUIET
;		endfor
;		; Remove all the merged
;		for i=0,nfiles-1 do begin
;		  filename = currentgroup[i]
;		  namepos=strpos(filename,'.',/reverse_search)
;		  rootname = strmid(filename,0,namepos)
;		  namepos=strpos(rootname,path_sep(),/reverse_search)
;		  rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
;		  print, 'Remove file '+rootname+'_merged.fits'
;		  FILE_DELETE, rootname+'_merged.fits', /ALLOW_NONEXISTENT, /QUIET
;		endfor
		; Remove the last coadded	      
;		filename = currentgroup[nfiles-1]
;		namepos=strpos(filename,'.',/reverse_search)
;		rootname = strmid(filename,0,namepos)
;		namepos=strpos(rootname,path_sep(),/reverse_search)
;		rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
;		print,'Remove file '+rootname+'_coadded.fits'
;		FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET	 
;	      endelse
;	    endelse
	  endif else begin
	    print,'You must select at least one file'
	  endelse
;	  widget_control,(*self.groupconf).refreshbut, sensitive=1
;          widget_control,(*self.groupconf).newbut, sensitive=1
;	  widget_control,(*self.groupconf).done, sensitive=1
;	  widget_control,(*self.groupconf).cancel, sensitive=1
;          widget_control,(*self.groupconf).doallbut, sensitive=1
;          widget_control,(*self.groupconf).dosetbut, sensitive=1
;	  widget_control,(*self.groupconf).filebase,sensitive=1
	  
	endif
        break
    end    
    ; done: close window and reduce all sets
    (*self.groupconf).doallbut:begin
	;if (*self.groupconf).stat ne 0 then begin
        ;    widget_control, event.top, /destroy
        ;    (*self.groupconf).stat=0
        ;endif
	; Now we load all the groups in the drip one after the other
	ngroups = self.objgroup->getdata(/ngroups)
	fgroups = self.objgroup->getdata(/fgroups)
	kgroups = self.objgroup->getdata(/kgroups)
	groupcriteria = self.objgroup->getdata(/criteria)
	ncriteria = n_elements(groupcriteria)
	currentgroupsel = (*self.groupconf).groupsel
	
	; Record the x and y offsets of the windows
	auxgeometry = widget_info((*self.groupconf).top,/GEOMETRY)
	(*self.groupconf).topxpos = auxgeometry.xoffset
	(*self.groupconf).topypos = auxgeometry.yoffset
	      
	(*self.groupconf).savecleanbase = widget_base((*self.groupconf).savebase,/column)
	proclabel = widget_label((*self.groupconf).savecleanbase, value='Start processing groups',xsize=400)
	
	widget_control,(*self.groupconf).top, $

	xoffset=300, yoffset=0;xoffset=(*self.groupconf).topxpos, yoffset=0;(*self.groupconf).topypos
		     
	for groupsel=currentgroupsel,ngroups-1 do begin
	  self->new,{id:0, top:1, handler:0, select:1}
	  ;self->reset
	  currentgroup = *(fgroups[groupsel])
	  nfiles = n_elements(currentgroup)
	  widget_control,proclabel,set_value='Processing group #'+strtrim(groupsel,1)+'/'+strtrim(ngroups-1,1)+ $
	       ' -> '+strtrim(nfiles,1)+' file(s)'
	  self->open, {id:0, top:1, handler:0, select:1}, filelist=currentgroup
	  self->run, {id:0, top:1, handler:0, select:1}, /BASIC_SCIENCE
	  
	  straux_criteria = ''
	  straux_gcriteria = ''
	  for countkey=0,ncriteria-1 do begin
	    straux_criteria = straux_criteria + " " +groupcriteria[countkey]
	    straux_gcriteria = straux_gcriteria + " " +kgroups(groupsel,countkey)
	  endfor
	  ; remove the appropriate data
	  msg = ["GROUP #"+strtrim(groupsel,1)+" was just reduced", $
	         straux_criteria, $
	         straux_gcriteria, $
	         ""]
	  ; Remove all the coadds except the last one
	  shownfiles = nfiles
	  if shownfiles gt 20 then shownfiles = 20
	  for i=0,nfiles-1 do begin
	    filename = currentgroup[i]
	    namepos=strpos(filename,'.',/reverse_search)
	    rootname = strmid(filename,0,namepos)
	    namepos=strpos(rootname,path_sep(),/reverse_search)
	    rootname = strmid(rootname,namepos+1,strlen(rootname))
	    msg = [msg,rootname]
	  endfor
	  if shownfiles gt 20 then msg = [msg, "..."]
	  
	  msg = [msg, $
	         ["", $
		 "                          Were this data useful?", $
	         "", $
	         "If YES DRIP will remove the undistorted images and coadds except for the last one", $
		 "If NO DRIP will remove the merged and coadded images", $
		 "If CANCEL DRIP will not remove any data"] $
		]
	  useful=dialog_message(msg,/question,/center,/cancel)
	  
	  ; Remove all the coadds except the last one
	  for i=0,nfiles-2 do begin
	    filename = currentgroup[i]
	    namepos=strpos(filename,'.',/reverse_search)
	    rootname = strmid(filename,0,namepos)
	    namepos=strpos(rootname,path_sep(),/reverse_search)
	    rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	   ; print, 'Remove file '+rootname+'_coadded.fits'
	    FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET
	  endfor
	  
	  if useful eq 'Yes' then begin
	    ; Here we delete all the undistort corresponding to these files 
	    ; And the coadds except for the last one
	    for i=0,nfiles-1 do begin
	      filename = currentgroup[i]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	    ;  print, 'Remove file '+rootname+'_undistorted.fits'
	      FILE_DELETE, rootname+'_undistorted.fits', /ALLOW_NONEXISTENT, /QUIET
	    endfor
	    if nfiles eq 1 then begin
	     ; print,'Remove file '+rootname+'_coadded.fits'
	      FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET
	    endif
	  endif else begin ; useful eq 'No' or 'Cancel'
	    if useful eq 'No' then begin
	      ; Here we delete all the merge and the coadds
	      for i=0,nfiles-1 do begin
		filename = currentgroup[i]
		namepos=strpos(filename,'.',/reverse_search)
		rootname = strmid(filename,0,namepos)
		namepos=strpos(rootname,path_sep(),/reverse_search)
		rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
		;print, 'Remove file '+rootname+'_merged.fits'
		FILE_DELETE, rootname+'_merged.fits', /ALLOW_NONEXISTENT, /QUIET
	      endfor
	      filename = currentgroup[nfiles-1]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	     ; print,'Remove file '+rootname+'_coadded.fits'
	      FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET
	    endif else begin
	      ; if cancel we remove all the data and we stop the processing
	      ; Remove all the undistorted
	      for i=0,nfiles-1 do begin
		filename = currentgroup[i]
		namepos=strpos(filename,'.',/reverse_search)
		rootname = strmid(filename,0,namepos)
		namepos=strpos(rootname,path_sep(),/reverse_search)
		rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
		;print, 'Remove file '+rootname+'_undistorted.fits'
		FILE_DELETE, rootname+'_undistorted.fits', /ALLOW_NONEXISTENT, /QUIET
	      endfor
	      ; Remove all the merged
	      for i=0,nfiles-1 do begin
		filename = currentgroup[i]
		namepos=strpos(filename,'.',/reverse_search)
		rootname = strmid(filename,0,namepos)
		namepos=strpos(rootname,path_sep(),/reverse_search)
		rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	;	print, 'Remove file '+rootname+'_merged.fits'
		FILE_DELETE, rootname+'_merged.fits', /ALLOW_NONEXISTENT, /QUIET
	      endfor
	      ; Remove the last coadded	      
	      filename = currentgroup[nfiles-1]
	      namepos=strpos(filename,'.',/reverse_search)
	      rootname = strmid(filename,0,namepos)
	      namepos=strpos(rootname,path_sep(),/reverse_search)
	      rootname = self.savepath + strmid(rootname,namepos+1,strlen(rootname))
	 ;     print,'Remove file '+rootname+'_coadded.fits'
	      FILE_DELETE, rootname+'_coadded.fits', /ALLOW_NONEXISTENT, /QUIET	      
	      break
	    endelse
	  endelse
	endfor
	
	; Clean memory - remove fgroups heaps
	self.objgroup->fgroups_clean,fgroups
	widget_control,(*self.groupconf).savecleanbase,/destroy
        break
    end
    ; cancel: close window
    (*self.groupconf).cancel:begin
	if (*self.groupconf).stat ne 0 then begin
            widget_control, event.top, /destroy
            (*self.groupconf).stat=0
        endif
        break
    end
    else: begin
          k = where((*((*self.groupconf).grouplist)) eq event.id)
	  if k(0) ne -1 then begin
	    if k(0) ne (*self.groupconf).groupsel then begin
	      (*self.groupconf).groupsel = k(0)
	      ; Record the x and y offsets of the windows
	      auxgeometry = widget_info((*self.groupconf).top,/GEOMETRY)
	      (*self.groupconf).topxpos = auxgeometry.xoffset
	      (*self.groupconf).topypos = auxgeometry.yoffset
	      
	      ; Remove previous list of files
	      widget_control, (*self.groupconf).listbase, /destroy
	      
	      ; Create the list of files for the selected group
              (*self.groupconf).listbase=widget_base((*self.groupconf).filebase,/column,/scroll,X_SCROLL_SIZE=300,Y_SCROLL_SIZE=500)
	      fgroups = self.objgroup->getdata(/fgroups)
	      nfiles = n_elements(*(fgroups[(*self.groupconf).groupsel]))
	      filelabel = widget_label((*self.groupconf).listbase,value=strtrim(nfiles,1)+' FILES in GROUP '+strtrim((*self.groupconf).groupsel,1),/frame)
	      (*((*self.groupconf).filelist)) = lonarr(nfiles)	 
	      fbasemain = widget_base((*self.groupconf).listbase,/row)
	      fsubbase = widget_base(fbasemain,/column,/NonExclusive)
	      trackcolumn = 0
	      *((*self.groupconf).fileval) = replicate(1,nfiles)
	      for i=0,nfiles-1 do begin
		filename = (*(fgroups[(*self.groupconf).groupsel]))[i]
		namepos=strpos(filename,path_sep(),/reverse_search)
		fname = strmid(filename,namepos+1,strlen(filename))
		pname = strmid(filename,0,namepos)
		(*((*self.groupconf).filelist))[i] = widget_button(fsubbase, $
                    value=fname, $
                    event_pro='drip_eventhand', $
                    uvalue={object:self, method:'opengroup'}, $
		    Tooltip=pname)
		if inallpush eq 1 then begin
		  widget_control,(*((*self.groupconf).filelist))[i],/set_button
		endif else begin
		  widget_control,(*((*self.groupconf).filelist))[i],set_button=0
		  (*((*self.groupconf).fileval))[i] = 0
		endelse
		;if i gt trackcolumn + nfilescolumn then begin
	        ;  fsubbase = widget_base(fbasemain,/column,/NonExclusive)
		;  trackcolumn = trackcolumn + nfilescolumn
		;endif
	      endfor
	      
	      ; Clean memory - remove fgroups heaps
	      self.objgroup->fgroups_clean,fgroups
	    endif
	    widget_control,(*self.groupconf).top, $

	    xoffset=300, yoffset=0;xoffset=(*self.groupconf).topxpos, yoffset=0;(*self.groupconf).topypos

	  endif
	  
          k = where((*((*self.groupconf).filelist)) eq event.id)
	  if k(0) ne -1 then begin
	    (*((*self.groupconf).fileval))[k(0)] = 1 - (*((*self.groupconf).fileval))[k(0)]
	  endif
	  
	  k = where((*self.groupconf).savelist eq event.id)
	  if k(0) ne -1 then begin
	    (*self.groupconf).saveval[k(0)] = 1 - (*self.groupconf).saveval[k(0)]
	  endif	         
	break
    end
  endswitch
  
  return
  
  ; Pop-up window to allow user to select group
  ;self->selectgroup,{id:0, top:1, handler:0, select:1}  

END

;******************************************************************************
;     OPEN - pick files for reduction
;******************************************************************************

pro drip_pipeman::open, event, filelist=filelist

;let user choose file if filelist is not input
if keyword_set(filelist) eq 0 then begin
  filelist=dialog_pickfile(filter=*self.loadfilter, file=self.lastfile, $
	/fix_filter, /multiple_files, /must_exist, /read, get_path=path, $
	path=self.loaddatapath, dialog_parent=event.top)
endif else begin
  path = self.loaddatapath
endelse
	  
if (filelist[0] ne '') then begin
    ; if n==0, make new drip object with file list, initialize file list
    if self.n eq 0 then begin
        drip=drip_new(filelist) ;create drip object
        if drip ne obj_new() then begin	    
	    ; update save paths in the drip according to the one in the gui
	    drip->setdata,pathsave=self.savepath
	    drip->setdata,pathcalsave=self.savecalpath
	
            ; store new data in local variables
            self.drip=drip
            *self.filelist=filelist
            self.n=n_elements(filelist)
            self.mode=drip->getdata(/mode)
            ; make name for new pipeline
            fits_read, filelist[0], null, header, /header
            detector=strtrim(sxpar(header,'DETECTOR'),2)
            self.name='FC_'+self.mode+'_'+detector+'_'+ $
                      strtrim(string(self.next),2)
            self.next=self.next+1
        endif
    ; add files to existing file list
    endif else begin
        file=*self.filelist
        file=[file, filelist]
        *self.filelist=file
        self.n=n_elements(file)
    endelse
    
    ; update display with first opened file
    kdisp = where(OBJ_VALID(*self.disp_sels) eq 1)
    if kdisp(0) ne -1 then begin
      dispobj = ((*self.disp_sels)[kdisp(0)]).disp
      im = readfits((*self.filelist)[self.ind])
      ;set 24-bit color indices
      device,decomposed=1
      if (size(im))[0] gt 0 then begin
        textdraw = (*self.filelist)[self.ind]
        fnamepos = strpos(textdraw,path_sep())
        if fnamepos ge 0 then $
          textdraw = strmid(textdraw,strpos(textdraw,path_sep(),/reverse_search)+1,strlen(textdraw))
        
        sd = size(im)
	if sd(0) eq 2 then dispobj->imageset,im,textdraw,'coco'
	if sd(0) eq 3 then dispobj->imageset,im(*,*,0),textdraw,'coco'
      endif
      device,decomposed=0	  
    endif
    
    ; print message
    if self.n gt 0 then begin
        sz=size(filelist, /n_elements)
        if sz gt 1 then self.mw->print, 'files ' + $
          file_basename(filelist[0]) + ' to ' + $
          file_basename(filelist[sz-1]) + ' added to filelist' $
        else self.mw->print, 'file ' + file_basename(filelist) + ' added to filelist'
	self->setpipepanel,'open'
    endif else begin
        msg='Error: open could not initialize pipe from ' + $
          file_basename(filelist[0])
        self.mw->print, msg
        s=dialog_message(msg)
    endelse
    ; store path and loadfilter
    self.loaddatapath=path ;store path
    self.lastfile=filelist[sz-1]
    if strmid(self.lastfile,strlen(self.lastfile)-5) eq '.fits' then $
      *self.loadfilter=['*.fits','*.fit','*.fts']
    if strmid(self.lastfile,strlen(self.lastfile)-4) eq '.fit' then $
      *self.loadfilter=['*.fit','*.fits','*.fts']
    if strmid(self.lastfile,strlen(self.lastfile)-4) eq '.fts' then $
      *self.loadfilter=['*.fts','*.fits','*.fit']
endif
end

;******************************************************************************
;     RUN - reduce data (takes care of all unreduced files in filelist)
;******************************************************************************

pro drip_pipeman::run, event, BASIC_SCIENCE=basic_science
; print message if nothing to do
if (self.ind ge self.n) then begin
    self.mw->print, 'End of File List.  No more files to reduce.'
    return
endif

initial_index = self.ind
; reduce the unreduced files
repeat begin ;reduce, display, increment
    widget_control, /hourglass
    ; get filename from filelist (for messages)
    fileonly=file_basename((*self.filelist)[self.ind])
    ; reduce data -> get new pipe steps
    self.mw->print, 'Reducing file ' + fileonly
    if initial_index eq self.ind then self.drip->run,(*self.filelist)[self.ind], /resetlist $
    else self.drip->run,(*self.filelist)[self.ind]
    self.mw->print, 'Finished reducing file ' + fileonly        
    pipe_steps=self.drip->getdata()
    self.ind=self.ind+1
    self->setpipepanel,'reduce'
    
    ; save file if auto-save is on
    if (*self.saveaction).autosav eq 1 then begin
      self.mw->print, 'Auto-Save is ON: Saving products according to configuration'
      if keyword_set(basic_science) then self->saveproducts_BS $
      else self->saveproducts
    endif
    
    ; copy data to dataman
    self.dataman->setdap,self.name,pipe_steps
    ; add data to sum (check if it's first reduced file ind==1)
    if self.ind eq 1 then begin
        ; make sum copy in dataman
        sum_name=self.name+'_sum'
        self.dataman->setdap,sum_name,pipe_steps
        ; set number of reduced files
        self.cnt=1
    endif else begin
    ; add data to existing sum
        ; get sum dap
        sum_name=self.name+'_sum'
        sum_dap=self.dataman->getdap(sum_name)
        ; check if it's valid, else make a new sum dap
        if ptr_valid(sum_dap) eq 0 then begin
            self.dataman->setdap,sum_name,pipe_steps
            self.cnt=1
        endif else begin
            ntags=n_tags(pipe_steps)
            for i=0,ntags-1 do begin
                if size(pipe_steps.(i),/n_dimensions) gt 1 then $
                  (*sum_dap).(i)=(*sum_dap).(i)+pipe_steps.(i)
            endfor
            self.cnt=self.cnt+1
        endelse
     endelse
    ; set displays if required
    dispn=size(*self.disp_sels,/n_elements)
    for dispi=0,dispn-1 do begin
       if (*self.dispnewpipe)[dispi] ne 'No Update' then $
          (*self.disp_sels)[dispi]->newdap, $
          dapnew=self.name, elemnew=(*self.dispnewpipe)[dispi]
    endfor
    ; update channels
    self.dataman->channelcall
    ; new reduced data so set flag to 0
    if (*self.saveaction).autosav eq 0 then self.saveflag=0
endrep until (self.ind ge self.n)
end

;******************************************************************************
;     STEPBACK - undo last reduction (only works once)
;                It can be a step back on the last image or last list of images
;                The function checks the label of the event calling it
;                   - "Undo Image" -> Step back one image
;                   - "Undo List" -> Step back last list of images
;******************************************************************************

pro drip_pipeman::stepback, event
 ; print,'gui_stepback - started, self.cnt=',self.cnt
  widget_Control, event.id, get_value=butlabel
  
  case butlabel of
    'Undo Image': onestep = 1
    'Undo List': onestep = 0
    'Re-Reduce (last file)': onestep = 1
    'Re-Reduce (last list of files)': onestep = 0
    else: onestep = 1
  endcase
    
  if self.cnt gt 0 then begin
      ; step back in drip (image or list according to button)
      if onestep eq 1 then begin
        self.drip->stepback 
	self.ind=self.ind-1
      endif else begin
        self.drip->stepback,/list
	self.ind=self.drip->getdata(/lastlistN)
      endelse
      
      ; update visibility of buttons
      self->setpipepanel,'stepback'
      self->setpipepanel,'open'
      ; get last pipe and sum DAP
      pipe_dap=self.dataman->getdap(self.name)
      sum_dap=self.dataman->getdap(self.name+'_sum')
      if (ptr_valid(pipe_dap) gt 0) and (ptr_valid(sum_dap) gt 0) then begin
          ; subtract pipe from sum for all elements with >1 dimensions
          ntags=n_tags(*pipe_dap)
	  for i=0,ntags-1 do begin
	      if size((*pipe_dap).(i),/n_dimensions) gt 1 then begin
        	(*sum_dap).(i)=(*sum_dap).(i)-(*pipe_dap).(i)
		spipe = size((*pipe_dap).(i))
		if spipe[0] eq 2 then (*pipe_dap).(i) = replicate(0.,spipe[1],spipe[2]) $
		else (*pipe_dap).(i) = replicate(0.,spipe[1],spipe[2],spipe[3])
	      endif
          endfor
          ; adjust file counter
          self.cnt=self.cnt-1
      endif
      ; set stepped back data
      pipe_steps=self.drip->getdata()
      self.dataman->setdap,self.name,pipe_steps
      self.dataman->channelcall
      ; message
      self.mw->print,'Reduction of last file was undone'
  endif else self.mw->print,'Can not undo as no file has been reduced'

end

;******************************************************************************
;     RE_REDUCE - re-reduce last file (undo previous reduction of it)
;                 WARNING: if there are files that are open but not
;                 reduced, then these files will be reduced as well
;******************************************************************************

pro drip_pipeman::re_reduce, event
 ; print,'gui_re-reduce - started, self.cnt=',self.cnt
  
  widget_control,event.id,get_value=butlabel
  
    if self.cnt gt 0 then begin
      ; run stepback
      self->stepback,event
      
      ; add last reduced file to filelist
      ; This is not valid anymore because I add it in the step back function
      ;self.ind=self.ind-1
      ; run pipe with unreduced files
      self->run,event
    endif else self.mw->print,'Can not re-reduce as no files have been reduced'

end

;******************************************************************************
;     SAVE - save result from current drip (uses drip::save)
;******************************************************************************

pro drip_pipeman::save, event
if (self.ind gt 0) then begin
    ; make default filename
    filename=(*self.filelist)[self.ind-1]
    namepos=strpos(filename,'.fit',/reverse_search)
    filename=strmid(filename,0,namepos)+'_reduced.fits'  ;or '_stacked.fits' (etc.)
    ; querry user for filename
    filename=dialog_pickfile(file=filename, /fix_filter, /write, $
                             filter=['*.fits','*.fit','*.fts'], $
                             get_path=path, path=self.savepath)
    ; save file

    if strlen(filename) gt 0 then begin

        self.savepath=path
        ; /stacked to save 'stacked' pipestep image
        self.drip->save, filename=filename  ; or , /stacked  (etc.)
        self.mw->print, 'saved reduced image.'
        self.saveflag=1

     endif else begin           ;CCB 7/20/12 else is case for when cancel is clicked
        ;print,"monkey"          ;Can't get it to just close the dialog_pickfile window
                                ;still kills the whole program
        ; widget_control, event.top, /destroy
     endelse
 endif else self.mw->print,'No new reduced files to save'

end

;******************************************************************************
;     SAVEPRODUCTS_BS - save final and intermediate data from current drip using config
;                       This has been added for basic science in order to save the required 
;                       data merged, undistorted, coadded and cube until merge
;                       I recommend to do something more elaborate after we are done with BS
;******************************************************************************

pro drip_pipeman::saveproducts_BS, event
  if (self.ind gt 0) then begin
        
  reducedlist = ["CLEANED", $
        	 "DROOPED", $
        	 "IMGLINEARIZED", $
		 "LINEARIZED", $
		 "FLATTED", $
        	 "STACKED", $
		 "UNDISTORTED", $
		 "MERGED"]
  callist = ["BADMAP", $
             "DARKS",  $
	     "FLATS",  $
	     "LINEAR_CORRECTION", $
	     "CLEANED_DARKS", $
             "DARKSUM", $
	     "CLEANED_FLATS", $
	     "DROOPED_FLATS", $
	     "IMGLINEARIZED_FLATS", $
	     "MASTERFLAT"]
    innames = [reducedlist,callist]
    self.drip->saveproducts, innames, output='BOTHCUBE'
    
    innames = ["UNDISTORTED","MERGED","COADDED"]
    self.drip->saveproducts, innames
endif else self.mw->print,'No new reduced files to save'
end

;******************************************************************************
;     SAVEPRODUCTS - save final and intermediate data from current drip using config
;******************************************************************************

pro drip_pipeman::saveproducts, event
if (self.ind gt 0) then begin
        
    ; make list of names of images to be saved
    innames = ''
    kcal = where((*((*self.saveconfstat).calval)) eq 1)
    if kcal(0) ne -1 then begin
      calnames = (*((*self.saveconfstat).callist))[kcal]
      innames = calnames
    endif
    kred = where((*((*self.saveconfstat).redval)) eq 1)
    if kred(0) ne -1 then begin
      rednames = (*((*self.saveconfstat).reducedlist))[kred]
      if innames[0] ne '' then innames = [calnames,rednames] $
      else innames = rednames
    endif
    
    ;make output
    outtype = 'NONE'
    if (*self.saveconfstat).makecube eq 1 then begin
      outtype = 'CUBE'
    endif else begin
      if ((*self.saveconfstat).redmakecube eq 1 && (*self.saveconfstat).calmakecube eq 1) then outtype = 'BOTHCUBE' $
      else if ((*self.saveconfstat).redmakecube eq 1) then outtype = 'REDCUBE' $
      else if ((*self.saveconfstat).calmakecube eq 1) then outtype = 'CALCUBE' 
    endelse
    
    self.drip->saveproducts, innames, output=outtype
endif else self.mw->print,'No new reduced files to save'
end

;******************************************************************************
;     DRIPCONF_EDIT - edit pipeline configuration
;******************************************************************************

pro drip_pipeman::dripconf_edit, event
; get drip configuration
common drip_config_info, dripconf
; open the edit window
edit_string_list, dripconf, $
  comment='Edit Drip Configuration:'
end

;******************************************************************************
;     PIPECONF - event function for displays configuration dialog window
;******************************************************************************

pro drip_pipeman::pipeconf, event, extramsg=extramsg
common gui_os_dependent_values, largefont, smallfont
common drip_config_info, dripconf
; Execute widget events
switch event.id of
    ; Menu: make popup window
    (*self.pipeconfstat).confbut:
    ; Menu: make popup window
    (*self.pipeconfstat).confmenu:begin
        ; Check if window is closed (i.e. stat=0) and dispn > 0
        if ((*self.pipeconfstat).stat eq 0) then begin
            ; make label and text widgets
            top=widget_base(/column)
            label=widget_label(top,font=largefont,value='Pipe Options:')
            info0=widget_label(top,value='The following selections are set')
            info1=widget_label(top,value='in drip conf by pressing [Done]')
	    if keyword_set(extramsg) then begin
	      infoextra = widget_label(top,value='')
	      for i=0, n_elements(extramsg) -1 do $
		infoextra = widget_label(top,value=extramsg[i])
	    endif

            ; make widgets for selecting jbclean status
            jbcbase=widget_base(top,/column,/frame)
            label=widget_label(jbcbase,value='Jail Bar Clean Selection:')
            jbclistbase=widget_base(jbcbase,/column,/Exclusive)
            jbclen=n_elements((*self.pipeconfstat).jbclist)
            for i=0,jbclen-1 do begin
               (*self.pipeconfstat).jbcsel[i]=widget_button(jbclistbase, $
                  value=(*self.pipeconfstat).jbclabel[i], $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'pipeconf'} )
            endfor
            ; get jbclean status from dripconf and set it
            ; !! If the keyword contains a non-registered value, no option !!
            ; !! will be selected in the configuration window.             !!
            jbc=getpar(dripconf,'jbclean')
            (*self.pipeconfstat).jbcval=jbc
            jbcind=(where((*self.pipeconfstat).jbclist eq jbc))[0]
            if jbcind gt -1 then $
               widget_control,(*self.pipeconfstat).jbcsel[jbcind],set_button=1

            ; make widgets for selecting cormerge status
            cmbase=widget_base(top,/column,/frame)
            label=widget_label(cmbase,value='Cor/Merge Register Selection:')
            cmlistbase=widget_base(cmbase,/column,/Exclusive)
            cmlen=n_elements((*self.pipeconfstat).cmlist)
            for i=0,cmlen-1 do begin
               (*self.pipeconfstat).cmsel[i]=widget_button(cmlistbase, $
                  value=(*self.pipeconfstat).cmlabel[i], $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'pipeconf'} )
            endfor
            ; get cormerge status from dripconf and set it
            ; !! If the keyword contains a non-registered value, no option !!
            ; !! will be selected in the configuration window.             !!
            cm=getpar(dripconf,'cormerge')
            (*self.pipeconfstat).cmval=cm
            cmind=(where((*self.pipeconfstat).cmlist eq cm))[0]
            if cmind gt -1 then $
               widget_control,(*self.pipeconfstat).cmsel[cmind],set_button=1

            ; make widgets for selecting corcoadd status
            ccbase=widget_base(top,/column,/frame)
            label=widget_label(ccbase,value='Cor/Coadd Register Selection:')
            cclistbase=widget_base(ccbase,/column,/Exclusive)
            cclen=n_elements((*self.pipeconfstat).cclist)
            for i=0,cclen-1 do begin
               (*self.pipeconfstat).ccsel[i]=widget_button(cclistbase, $
                  value=(*self.pipeconfstat).cclabel[i], $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'pipeconf'} )
            endfor
            ; get corcoadd status from dripconf and set it
            cc=getpar(dripconf,'CORCOADD')
            (*self.pipeconfstat).ccval=cc
            ccind=(where((*self.pipeconfstat).cclist eq cc))[0]
            if ccind gt -1 then $
               widget_control,(*self.pipeconfstat).ccsel[ccind],set_button=1
           
            ;make widgets for spectral extraction mode
            spexbase=widget_base(top,/frame,/column)
            label4=widget_label(spexbase,value='Select Spectral Extraction Mode')
            spexlistbase=widget_base(spexbase,/Exclusive)
            spexlen=n_elements((*self.pipeconfstat).spexlist)
            for i=0,spexlen-1 do begin
               (*self.pipeconfstat).spexsel[i]=widget_button(spexlistbase, $
                  value=(*self.pipeconfstat).spexlabel[i], $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'pipeconf'} )
            endfor
           ; get extmode status from dripconf and set it
            ; !! If the keyword contains a non-registered value, no option !!
            ; !! will be selected in the configuration window.             !!
            spex=getpar(dripconf,'extmode')
            (*self.pipeconfstat).spexval=spex
            spexind=(where((*self.pipeconfstat).spexlist eq spex))[0]
            if spexind gt -1 then $
               widget_control,(*self.pipeconfstat).spexsel[spexind],set_button=1


            ; make button widgets
            row=widget_base(top,/row)
            (*self.pipeconfstat).done=widget_button(row, value='Done', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'pipeconf'} )
            (*self.pipeconfstat).cancel=widget_button(row, value='Cancel', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'pipeconf'} )
            ; set status
            (*self.pipeconfstat).stat=1
            ; realize and start widgets

            widget_control, top, /realize, xoffset=300, yoffset=50

            xmanager, 'Configure Pipeline->Displays', top

        endif
        break
    end
    ; Done: close window and save settings
    (*self.pipeconfstat).done:begin
        if (*self.pipeconfstat).stat ne 0 then begin
           ; change settings
           setpar, dripconf, 'jbclean',(*self.pipeconfstat).jbcval
           setpar, dripconf, 'cormerge',(*self.pipeconfstat).cmval
           setpar, dripconf, 'corcoadd',(*self.pipeconfstat).ccval

           setpar, dripconf, 'extmode', (*self.pipeconfstat).spexval
           ; close window, reset status
           widget_control, event.top, /destroy
           (*self.pipeconfstat).stat=0
        endif
        break
    end
    ; Cancel: close window
    (*self.pipeconfstat).cancel:begin
        if (*self.pipeconfstat).stat ne 0 then begin
            widget_control, event.top, /destroy
            (*self.pipeconfstat).stat=0
        endif
        break
    end
    else:begin ; look through widget lists and set options
       ;** look for jbclean updates
       jbcind=(where((*self.pipeconfstat).jbcsel eq event.id))[0]
       if jbcind gt -1 then begin
          ; set the new value
          (*self.pipeconfstat).jbcval= $
             (*self.pipeconfstat).jbclist[jbcind]
          break
       endif
       ;** look for cormerge updates
       cmind=(where((*self.pipeconfstat).cmsel eq event.id))[0]
       if cmind gt -1 then begin
          ; set the new value
          (*self.pipeconfstat).cmval= $
             (*self.pipeconfstat).cmlist[cmind]
       endif
       ;** look for corcoadd updates
       ccind=(where((*self.pipeconfstat).ccsel eq event.id))[0]
       if ccind gt -1 then begin
          ; set the new value
          (*self.pipeconfstat).ccval= $
             (*self.pipeconfstat).cclist[ccind]
       endif

       ;**look for extmode updates
       spexind=(where((*self.pipeconfstat).spexsel eq event.id))[0]
       if spexind gt -1 then begin
          ;set the new value
          (*self.pipeconfstat).spexval= $
             (*self.pipeconfstat).spexlist[spexind]
       endif

       break
    endelse
endswitch
end

;******************************************************************************
;     DISPCONF - event function for displays configuration dialog window
;******************************************************************************

pro drip_pipeman::dispconf, event
; Setup: number of displays and list of possible data frames, get fonts
dispn=size(*self.disp_sels,/n_elements)
framelist=['No Update','Data','Cleaned','Badflags','Flatted','Stacked', $
           'Undistorted','Merged','Coadded','Badmap','Masterflat']
framen=size(framelist,/n_elements)
common gui_os_dependent_values, largefont, smallfont
; Execute widget events
case event.id of
    ; Menu: make popup window
    (*self.dispconfstat).menu:begin
        ; Check if window is closed (i.e. stat=0) and dispn > 0
        if ((*self.dispconfstat).stat eq 0) and (dispn gt 0) then begin
            ; set selection indices of previous settings
            for dispi=0, dispn-1 do begin
                ind=where(framelist eq (*self.dispnewpipe)[dispi])
                if ind gt -1 then (*self.dispconfstat).dispsel[dispi]=ind $
                else (*self.dispconfstat).dispsel[dispi]=0
            endfor
            ; make label and text widgets
            top=widget_base(/column)
            label=widget_label(top,font=largefont,value='Display Options:')
            info0=widget_label(top,value='Pipeline steps set below will be')
            info1=widget_label(top,value='automatically displayed whenever')
            info2=widget_label(top,value='new data is reduced')
            ; make selection widgets
            for dispi=0, dispn-1 do begin
                title='Display '+string(byte('A')+byte(dispi))
                (*self.dispconfstat).dispdrop[dispi]=widget_droplist(top, $
                     value=framelist, event_pro='drip_eventhand', $
                     uvalue={object:self, method:'dispconf'}, $
                     title=title )
            endfor
            ; make button widgets
            row=widget_base(top,/row)
            (*self.dispconfstat).done=widget_button(row, value='Done', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'dispconf'} )
            (*self.dispconfstat).cancel=widget_button(row, value='Cancel', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'dispconf'} )
            ; set status
            (*self.dispconfstat).stat=1
            ; realize and start widgets

            widget_control, top, /realize, xoffset=300, yoffset=0

            for dispi=0, dispn-1 do $
              widget_control, (*self.dispconfstat).dispdrop[dispi], $
              set_droplist_select=(*self.dispconfstat).dispsel[dispi]
            xmanager, 'Configure Pipeline->Displays', top
            ;print,(*self.dispconfstat).dispsel
        endif
        break
    end
    ; Done: close window and save settings
    (*self.dispconfstat).done:begin
        if (*self.dispconfstat).stat ne 0 then begin
            ; change settings
            for dispi=0, dispn-1 do begin
                ind=(*self.dispconfstat).dispsel[dispi]
                (*self.dispnewpipe)[dispi]=framelist[ind]
            endfor
            ; close window, reset status
            widget_control, event.top, /destroy
            (*self.dispconfstat).stat=0
        endif
        break
    end
    ; Cancel: close window
    (*self.dispconfstat).cancel:begin
        if (*self.dispconfstat).stat ne 0 then begin
            widget_control, event.top, /destroy
            (*self.dispconfstat).stat=0
        endif
        break
    end
    else:begin ; look through drop down list and set drop option
        ind=where((*self.dispconfstat).dispdrop eq event.id)
        if ind gt -1 then $
          (*self.dispconfstat).dispsel[ind]=event.index
    endelse
endcase
end

;******************************************************************************
;     VIEWHEAD - Show Fits Header of Current Pipe
;******************************************************************************
pro drip_pipeman::viewhead, event
; Check if data is available
if self.cnt gt 0 then begin
    ; Get get correct header
    widget_control, event.id, get_value=value
    pipe_dap=self.dataman->getdap(self.name)
    if strpos(value,'Base') gt -1 then $
      head=(*pipe_dap).basehead else head=(*pipe_dap).header
    ; Open edit window
    edit_param_list, head, $
      comment='WARNING: this window is not updated as new images are loaded', $
      /viewonly
endif else begin
    ; No data available -> print message in message window
    self.mw->print, $
      'PipeMan::ViewHead - no header available, current pipe has no data'
endelse
end

;******************************************************************************
;     AUTOSAVE - Select Auto saving
;******************************************************************************
pro drip_pipeman::autosave, event
  
  (*self.saveaction).autosav = 1 - (*self.saveaction).autosav
  if (*self.saveaction).autosav eq 0 then begin
    widget_control,(*self.saveaction).savebut,sensitive=1  
  endif else begin
    widget_control,(*self.saveaction).savebut,sensitive=0
  endelse
  
end

;******************************************************************************
;     SAVECONF - Show and edit saving configuration
;******************************************************************************
pro drip_pipeman::saveconf, event, extramsg=extramsg
common gui_os_dependent_values, largefont, smallfont
common drip_config_info, dripconf

;if OBJ_VALID(self.drip) eq 0 then begin
;  drip_message, 'INFO: You must a file first'
;  return
;endif

; Execute widget events
switch event.id of
    ; Menu: make popup window
    (*self.saveconfstat).menu:
    (*self.saveconfstat).but:begin
        ; Check if window is closed (i.e. stat=0) and dispn > 0
        if ((*self.saveconfstat).stat eq 0) then begin
            ; make label and text widgets
            top=widget_base(/column)
            label=widget_label(top,font=largefont,value='SAVE Configuration:')
            info0=widget_label(top,value='Select what products would you like to save when pressing')
            info2=widget_label(top,value='the Save button in the main window or menu')
	    if keyword_set(extramsg) then begin
	      infoextra = widget_label(top,value='')
	      for i=0, n_elements(extramsg) -1 do $
		infoextra = widget_label(top,value=extramsg[i])
	    endif
	    	    
	    calredbase = widget_base(top,/row)
            ; make widgets for selecting calibration products to be saved
            calbase=widget_base(calredbase,/column,/frame)
            callabel=widget_label(calbase,value='Calibration Products:')
            callistbase=widget_base(calbase,/column,/NonExclusive)
	    (*self.saveconfstat).calallid=widget_button(callistbase, $
               value="ALL", $
               event_pro='drip_eventhand', $
               uvalue={object:self, method:'saveconf'} )
            ncal=n_elements(*((*self.saveconfstat).callist))
            for i=0,ncal-1 do begin
               (*((*self.saveconfstat).calsel))[i]=widget_button(callistbase, $
                  value=(*((*self.saveconfstat).callist))[i], $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'saveconf'} )
	       if (*((*self.saveconfstat).calval))[i] eq 1 then $
	         widget_control,(*((*self.saveconfstat).calsel))[i],/set_button
            endfor
	    kallcal = where((*((*self.saveconfstat).calval)) eq 0 )
	    if kallcal(0) eq -1 then begin
	      widget_control,(*self.saveconfstat).calallid,/set_button
	      (*self.saveconfstat).calallsel = 1
            endif
	    calcubebase=widget_base(calbase,/column,/frame,/NonExclusive)
	    ((*self.saveconfstat).calcubeid)=widget_button(calcubebase, $
                  value='Make Cube with Calibration Data?', $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'saveconf'} )
	    if (*(self.saveconfstat)).calmakecube eq 1 then $
	      widget_control,(*(self.saveconfstat)).calcubeid,/set_button
	    
	    
            ; make widgets for selecting reduced products to be saved
            redbase=widget_base(calredbase,/column,/frame)
            redlabel=widget_label(redbase,value='Reduction Products:')
            redlistbase=widget_base(redbase,/column,/NonExclusive)
	    (*self.saveconfstat).redallid=widget_button(redlistbase, $
               value="ALL", $
               event_pro='drip_eventhand', $
               uvalue={object:self, method:'saveconf'} )
            nred=n_elements(*((*self.saveconfstat).reducedlist))
            for i=0,nred-1 do begin
               (*((*self.saveconfstat).redsel))[i]=widget_button(redlistbase, $
                  value=(*((*self.saveconfstat).reducedlist))[i], $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'saveconf'} )
	       if (*((*self.saveconfstat).redval))[i] eq 1 then $
	         widget_control,(*((*self.saveconfstat).redsel))[i],/set_button
            endfor
	    kallred = where((*((*self.saveconfstat).redval)) eq 0 )
	    if kallred(0) eq -1 then begin
	      widget_control,(*self.saveconfstat).redallid,/set_button
	      (*self.saveconfstat).redallsel = 1
            endif
            redcubebase=widget_base(redbase,/column,/frame,/NonExclusive)
	    ((*self.saveconfstat).redcubeid)=widget_button(redcubebase, $
                  value='Make Cube with Reduced Data?', $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'saveconf'} )
	    if (*(self.saveconfstat)).redmakecube eq 1 then $
	      widget_control,(*(self.saveconfstat)).redcubeid,/set_button
		  
	    ; Unique Data cube widgets
            cubebase=widget_base(top,/column,/frame,/NonExclusive)
	    ((*self.saveconfstat).cubeid)=widget_button(cubebase, $
                  value='Make A Unique Data Cube with All Calibratation and Reduced Data?', $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'saveconf'} )
	    if (*(self.saveconfstat)).makecube eq 1 then $
	      widget_control,(*(self.saveconfstat)).cubeid,/set_button
	    
	    ; make widgets for save directory selection
            dircalbase = widget_base(top,/row)
	    (*self.saveconfstat).calselbut=widget_button(dircalbase, value='Calibr.', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'saveconf'} )
	    ;(*self.saveconfstat).caliddir=widget_text(dircalbase, value=(*self.saveconfstat).caldir)
	    (*self.saveconfstat).caliddir=widget_text(dircalbase, value=self.savecalpath,xsize=60)
            dirredbase = widget_base(top,/row)
	    (*self.saveconfstat).redselbut=widget_button(dirredbase, value='Reduced', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'saveconf'} )
	    ;(*self.saveconfstat).rediddir=widget_text(dirredbase, value=(*self.saveconfstat).reddir)
	    (*self.saveconfstat).rediddir=widget_text(dirredbase, value=self.savepath,xsize=60)

            ; make button widgets
            row=widget_base(top,/row)
            (*self.saveconfstat).done=widget_button(row, value='Done', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'saveconf'} )
            (*self.saveconfstat).cancel=widget_button(row, value='Cancel', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'saveconf'} )
            ; set status
            (*self.saveconfstat).stat=1
            ; realize and start widgets

            widget_control, top, /realize, xoffset=300, yoffset=50

            xmanager, 'Configure Save->Displays', top
        endif
        break
    end
    ; Done: close window and save settings
    (*self.saveconfstat).done:begin
        if (*self.saveconfstat).stat ne 0 then begin
           ; update save directories in drip
	   if OBJ_VALID(self.drip) eq 1 then begin
	     if self.drip ne OBJ_NEW() then begin
	       self.drip->setdata,pathsave=self.savepath
	       self.drip->setdata,pathcalsave=self.savecalpath
	     endif
	   endif
           ; close window, reset status
           widget_control, event.top, /destroy
           (*self.saveconfstat).stat=0
        endif
        break
    end
    ; Calibr.: Change directory where reduced data will be stored
    (*self.saveconfstat).calselbut:begin
	path=dialog_pickfile(filter=*self.loadfilter, $
	      /fix_filter, /must_exist, /directory, $
	      title='Select Directory to Save Calibration Products', $
	      path=self.savecalpath, dialog_parent=event.top)
	
	self.savecalpath = path
	widget_control,(*self.saveconfstat).caliddir,set_value=self.savecalpath
        break
    end
    ; Reduce: Change directory where reduced data will be stored
    (*self.saveconfstat).redselbut:begin
	path=dialog_pickfile(filter=*self.loadfilter, $
	      /fix_filter, /must_exist, /directory, $
	      title='Select Directory to Save Reduced Data', $
	      path=self.savepath, dialog_parent=event.top)
	
	self.savepath = path
	widget_control,(*self.saveconfstat).rediddir,set_value=self.savepath
        break
    end
    ; Calibr: Select all products
    (*self.saveconfstat).calallid:begin
        (*self.saveconfstat).calallsel = 1 - (*self.saveconfstat).calallsel
	if (*self.saveconfstat).calallsel eq 1 then newval = 1 $
	else newval = 0
	ncal=n_elements(*((*self.saveconfstat).callist))
        for i=0,ncal-1 do begin
           widget_control,(*((*self.saveconfstat).calsel))[i],set_button=newval
	   (*((*self.saveconfstat).calval))[i]=newval
        endfor
	break
    end
    ; Reduce: Select all products
    (*self.saveconfstat).redallid:begin
        (*self.saveconfstat).redallsel = 1 - (*self.saveconfstat).redallsel
	if (*self.saveconfstat).redallsel eq 1 then newval = 1 $
	else newval = 0
	nred=n_elements(*((*self.saveconfstat).reducedlist))
        for i=0,nred-1 do begin
           widget_control,(*((*self.saveconfstat).redsel))[i],set_button=newval
	   (*((*self.saveconfstat).redval))[i]=newval
        endfor
	break
    end
    ; cancel: close window
    (*self.saveconfstat).cancel:begin
	if (*self.saveconfstat).stat ne 0 then begin
            widget_control, event.top, /destroy
            (*self.saveconfstat).stat=0
        endif
        break
    end
    ; "Make a cube with all the data" widget
    (*self.saveconfstat).cubeid:begin
	  (*(self.saveconfstat)).makecube = 1 - (*(self.saveconfstat)).makecube
        break
    end
    ; "Make a cube with calibration data" widget
    (*self.saveconfstat).calcubeid:begin
	  (*(self.saveconfstat)).calmakecube = 1 - (*(self.saveconfstat)).calmakecube
        break
    end
    ; "Make a cube with reduced data" widget
    (*self.saveconfstat).redcubeid:begin
	  (*(self.saveconfstat)).redmakecube = 1 - (*(self.saveconfstat)).redmakecube
        break
    end
    else:begin ; look through widget lists and set options
          kcal = where((*((*self.saveconfstat).calsel)) eq event.id)
	  if kcal(0) ne -1 then begin
	    (*((*self.saveconfstat).calval))[kcal(0)] = 1-(*((*self.saveconfstat).calval))[kcal(0)]
	    
	    ; Check if we need to update the all button
	    kallcal = where((*((*self.saveconfstat).calval)) eq 0 )
	    if kallcal(0) eq -1 then begin
	      widget_control,(*self.saveconfstat).calallid,/set_button
	      (*self.saveconfstat).calallsel = 1
            endif
	    kallcal = where((*((*self.saveconfstat).calval)) eq 1 )
	    if kallcal(0) eq -1 then begin
	      widget_control,(*self.saveconfstat).calallid,set_button=0
	      (*self.saveconfstat).calallsel = 0
            endif
	  endif
          kred = where((*((*self.saveconfstat).redsel)) eq event.id)
	  if kred(0) ne -1 then begin
	    (*((*self.saveconfstat).redval))[kred(0)] = 1-(*((*self.saveconfstat).redval))[kred(0)]
	    
	    ; Check if we need to update the all button
	    kallred = where((*((*self.saveconfstat).redval)) eq 0 )
	    if kallred(0) eq -1 then begin
	      widget_control,(*self.saveconfstat).redallid,/set_button
	      (*self.saveconfstat).redallsel = 1
            endif
	    kallred = where((*((*self.saveconfstat).redval)) eq 1 )
	    if kallred(0) eq -1 then begin
	      widget_control,(*self.saveconfstat).redallid,set_button=0
	      (*self.saveconfstat).redallsel = 0
            endif
	  endif
    endelse
endswitch
end



;******************************************************************************
;     REMOVEOPEN - Show and select data to be removed from the open file list
;******************************************************************************
PRO drip_pipeman::removeopen, event
  
  common gui_os_dependent_values, largefont, smallfont
  common drip_config_info, dripconf

if OBJ_VALID(self.drip) eq 0 then begin
  drip_message, 'INFO: No active pipeline'
  return
endif

if self.n eq self.ind then begin
  drip_message, 'INFO: You must open a file first'
  return
endif

; Execute widget events
switch event.id of
    ; Menu: make popup window
    (*self.removeconf).menu:
    (*self.removeconf).but:begin
        ; Check if window is closed (i.e. stat=0) and dispn > 0
        if ((*self.removeconf).stat eq 0) then begin
            ; make label and text widgets
            top=widget_base(/column)
            label=widget_label(top,font=largefont,value='REMOVE FILES')
            info1=widget_label(top,value='Select what products that you recently opened ')
            info2=widget_label(top,value='would you like to remove from the list when pressing Remove')
	    
	    ; Create list of files
	    nfiles = self.n - self.ind
	    (*((*self.removeconf).removelist)) = lonarr(nfiles)
	    (*((*self.removeconf).filesel)) = replicate(0,nfiles)
	    (*((*self.removeconf).butdraw)) = lonarr(nfiles)
	    
	    filebase = widget_base(top,/row)
	    dispbase = widget_base(filebase,/column)
            listbase=widget_base(filebase,/column,/NonExclusive)
	    
	    ; Button for selecting all images
	    (*self.removeconf).allbut = widget_button(listbase, $
                  value='ALL', $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'removeopen'} )
	    (*self.removeconf).allsel = 0 
	    dummywidg = widget_label(dispbase,value='',ysize=25)
	    for i=0,nfiles-1 do begin
	      (*((*self.removeconf).butdraw))[i] = widget_button(dispbase, $
                  value='Display', $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'removeopen'} )
	      (*((*self.removeconf).removelist))[i] = widget_button(listbase, $
                  value=(*self.filelist)[self.ind+i], $
                  event_pro='drip_eventhand', $
                  uvalue={object:self, method:'removeopen'} )
	    endfor
	    
	    ; make display
	    xysize = (*self.removeconf).xysize
	    drawbase = widget_base(top,/row)
	    ;(*self.removeconf).draw=widget_draw(drawbase, xsize=xysize+4, ysize=xysize+4, retain=2)
	    (*self.removeconf).draw=cw_drip_disp(drawbase, quad_id='F', xsize=xysize, ysize=xysize)
	    id=widget_info((*self.removeconf).draw, /child)
	    widget_control, id, get_uvalue=dispobj
	    (*self.removeconf).dispobj=dispobj
	    
	    ; make analysis objects for image scaling and create dispman
	    (*self.removeconf).dispman=obj_new('drip_dispman', (*self.removeconf).dispobj, self.dataman, 1, drawbase)
	    (*self.removeconf).analscale=obj_new('drip_analman_scale',top)
	    
	    widget_control, top, set_uvalue=[self.dataman,(*self.removeconf).dispman,self], /realize
            (*self.removeconf).dispman->start, (*self.removeconf).analscale
	    (*self.removeconf).analscale->start, (*self.removeconf).dispobj
	    
            ; make button widgets
            row=widget_base(top,/row)
            (*self.removeconf).done=widget_button(row, value='Remove', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'removeopen'} )
            (*self.removeconf).cancel=widget_button(row, value='Cancel', $
              event_pro='drip_eventhand', $
              uvalue={object:self, method:'removeopen'} )
            ; set status
            (*self.removeconf).stat=1
            ; realize and start widgets

            widget_control, top, /realize, xoffset=300;, yoffset=300

            xmanager, 'Select and Remove Files', top
        endif
        break
    end
    ; Done: close window and save settings
    (*self.removeconf).done:begin
        ; Remove required files
	nfiles = self.n - self.ind
	ksel = where((*((*self.removeconf).filesel)) eq 1)
	if ksel(0) ne -1 then begin
	  knosel = where((*((*self.removeconf).filesel)) eq 0)
	  if knosel(0) ne -1 then begin
	    openfiles = (*self.filelist)[self.ind:self.n-1]
	    newopenfiles = openfiles(knosel)
	    if self.ind eq 0 then *self.filelist = newopenfiles $
	    else *self.filelist = [(*self.filelist)[0:self.ind-1],newopenfiles]
	  endif else begin
	    if self.ind eq 0 then *self.filelist=[''] $
	    else *self.filelist = (*self.filelist)[0:self.ind-1]
	    ; update buton visibility 
	    self->setpipepanel,'remove'
	  endelse
	  self.n = self.n - n_elements(ksel)
	endif
	
	if (*self.removeconf).stat ne 0 then begin
           ; close window, reset status
           widget_control, event.top, /destroy
           (*self.removeconf).stat=0
	   obj_destroy,(*self.removeconf).dispman
	   obj_destroy,(*self.removeconf).analscale
        endif
        break
    end
    ; cancel: close window
    (*self.removeconf).cancel:begin
	if (*self.removeconf).stat ne 0 then begin
            widget_control, event.top, /destroy
            (*self.removeconf).stat=0
	   obj_destroy,(*self.removeconf).dispman
	   obj_destroy,(*self.removeconf).analscale
        endif
        break
    end
    (*self.removeconf).allbut: begin
          nfiles = self.n - self.ind
	  (*self.removeconf).allsel = 1-(*self.removeconf).allsel
	  for i=0,nfiles-1 do begin
	    (*((*self.removeconf).filesel))[i] = (*self.removeconf).allsel
	    widget_control,(*((*self.removeconf).removelist))[i],set_button=(*self.removeconf).allsel
	  endfor
    end
    else: begin; look through widget lists and set options
          k = where((*((*self.removeconf).removelist)) eq event.id)
	  if k(0) ne -1 then begin
	    (*((*self.removeconf).filesel))[k(0)] = 1-(*((*self.removeconf).filesel))[k(0)]
	  endif
	  
          k = where((*((*self.removeconf).butdraw)) eq event.id)
	  if k(0) ne -1 then begin
	    ; open image
	    im = readfits((*self.filelist)[self.ind+k(0)])
            ;set 24-bit color indices
            device,decomposed=1
	    if (size(im))[0] gt 0 then begin
	      textdraw = (*self.filelist)[self.ind+k(0)]
	      fnamepos = strpos(textdraw,path_sep())
	      if fnamepos ge 0 then begin
		 textdraw = strmid(textdraw,strpos(textdraw,path_sep(),/reverse_search)+1,strlen(textdraw))
	      endif	    
	    
	      (*self.removeconf).dispobj->imageset,im(*,*,0),textdraw,'coco'
	      ;widget_control, (*self.removeconf).draw, get_value=wid
	      ;wset, wid
              ;tv,im
	    endif
	    device,decomposed=0	    
	  endif
    endelse
endswitch
end

;******************************************************************************
;     MINIMIZE - Minimize the panel in the main window
;******************************************************************************
PRO drip_pipeman::minimize, event
  
  ; Read sensitivity/state of previous buttons
  
  ; Remove current panel
  widget_control,self.deletebase,/destroy
  
  self.deletebase = widget_base(self.mainbase, /row, /frame)
  pipepan = self.deletebase
  maxbut = widget_button(self.deletebase,value=">",$
                        	event_pro='drip_eventhand',$
                        	uvalue={object:self, method:'maximize' })
  ;lab=widget_label(self.deletebase, value="Click to expand pipe control",font=smallfont)
  new=widget_button(pipepan, value='Reset', event_pro='drip_eventhand', $
      uvalue={object:self, method:'new'}, /sensitive, $
      font=mediumfont, ysize=25) 
  open=widget_button(pipepan, value='Open', $
      event_pro='drip_eventhand', $
      uvalue={object:self, method:'opengroup'}, /sensitive, $
      font=mediumfont, ysize=25)
  cancelopen=widget_button(pipepan, value='Remove', $
      event_pro='drip_eventhand', $
      uvalue={object:self, method:'removeopen'}, /sensitive, $
      font=mediumfont, ysize=25)    
  stepback=widget_button(pipepan, value='< Back', event_pro='drip_eventhand', $
  	uvalue={object:self, method:'stepback'}, /sensitive, $
  	font=mediumfont, ysize=25,TOOLTIP='Undo Image')  
  stepbacklist=widget_button(pipepan, value='<< Back', event_pro='drip_eventhand', $
  	uvalue={object:self, method:'stepback'}, /sensitive, $
  	font=mediumfont, ysize=25,TOOLTIP='Undo List')
  reduce=widget_button(pipepan, value='Go', event_pro='drip_eventhand', $
      uvalue={object:self, method:'run'}, /sensitive, $
      font=mediumfont, ysize=25,TOOLTIP='Reduce')
  
  savepan = self.deletebase
  ;labsav=widget_label(savepan, value=" Saving Control: ", font=smallfont,/frame)
  savec=widget_button(savepan, value='Steps', event_pro='drip_eventhand', $
	uvalue={object:self, method:'saveconf'}, /sensitive, $
	font=mediumfont, ysize=25)
  saveres=widget_button(savepan, value='Save', event_pro='drip_eventhand', $
	uvalue={object:self, method:'saveproducts'}, sensitive=0, $
	font=mediumfont, ysize=25)
  autobase=widget_base(savepan, /row, /nonexclusive, $
	event_pro='drip_eventhand', uvalue={object:self, method:'scan'})
  autosave=widget_button(autobase, value='Auto-Save', event_pro= $
          'drip_eventhand', uvalue={object:self, method:'autosave'}, $
          font=mediumfont)
  
  ; update pipestat
  (*self.pipestat).resetpipe = new
  (*self.pipestat).open = open
  (*self.pipestat).removefile = cancelopen
  (*self.pipestat).reduce = reduce
  (*self.pipestat).stepback = stepback
  (*self.pipestat).stepbacklist = stepbacklist
      
  (*self.removeconf).but = cancelopen

  (*self.saveaction).autobut = autosave
  (*self.saveaction).savebut = saveres 
 
  (*self.saveconfstat).but  = savec
    
  ; update button states according to previous buttons
  self->setpipepanel,'same'
  if (*self.saveaction).autosav eq 0 then begin
    widget_control,(*self.saveaction).savebut,sensitive=1  
    widget_control,autosave,set_button=0
  endif else begin
    widget_control,(*self.saveaction).savebut,sensitive=0
    widget_control,autosave,set_button=1
  endelse
    
  self.maxview = 0
  
END


;******************************************************************************
;     MAXIMIZE - Maximize the panel in the main window
;******************************************************************************
PRO drip_pipeman::maximize, event

  ; Read sensitivity/state of previous buttons
  
  ; Remove current panel
  widget_control,self.deletebase,/destroy
  
  panelsbase = widget_base(self.mainbase,/column)
  self.deletebase = panelsbase

  ;---------
  ; on-screen pipeline control buttons
  pipepan = widget_base(panelsbase,/row, /frame)
  minbut = widget_button(pipepan,value="<",$
                        	event_pro='drip_eventhand',$
                        	uvalue={object:self, method:'minimize' })
  lab=widget_label(pipepan, value=" Pipeline Control : ", font=largefont) 

  newbase = widget_base(pipepan,/column)  
  new=widget_button(newbase, value='Reset Pipe', event_pro='drip_eventhand', $
	uvalue={object:self, method:'new'}, /sensitive, $
	font=mediumfont, ysize=25)
  ; configuration and setup options
  pipeconfidbut=widget_button(newbase, value='Pipe Conf', $
	event_pro='drip_eventhand', uvalue={object:self, method:'pipeconf'})
  filespan = widget_base(pipepan,/column,/frame)
  fileslabel = widget_label(filespan,value='File(s)')      
  filesbutpan = widget_base(filespan,/row)
  open=widget_button(filesbutpan, value='Open', $
	event_pro='drip_eventhand', $
	uvalue={object:self, method:'opengroup'}, /sensitive, $
	font=mediumfont, ysize=25)
  cancelopen=widget_button(filesbutpan, value='Remove', $
	event_pro='drip_eventhand', $
	uvalue={object:self, method:'removeopen'}, /sensitive, $
	font=mediumfont, ysize=25)
;test=widget_button(filespan, value='Test', $
;      event_pro='drip_eventhand', $
;      uvalue={object:self, method:'test'}, /sensitive, $
;      font=mediumfont, ysize=25)
  redbutpan = widget_base(pipepan,/column)
  reduce=widget_button(redbutpan, value='Reduce', event_pro='drip_eventhand', $
	uvalue={object:self, method:'run'}, /sensitive, $
	font=mediumfont, ysize=25)    
stepback=widget_button(redbutpan, value='Undo Image', event_pro='drip_eventhand', $
      uvalue={object:self, method:'stepback'}, /sensitive, $
      font=mediumfont, ysize=25)  
stepbacklist=widget_button(redbutpan, value='Undo List', event_pro='drip_eventhand', $
      uvalue={object:self, method:'stepback'}, /sensitive, $
      font=mediumfont, ysize=25)

  ;---------
  savepanall = widget_base(panelsbase,/row)
  savepan = widget_base(savepanall,/row, /frame)
  labsav=widget_label(savepan, value=" Data Saving Control: ", font=largefont)
  savec=widget_button(savepan, value='Step Definition', event_pro='drip_eventhand', $
	uvalue={object:self, method:'saveconf'}, /sensitive, $
	font=mediumfont, ysize=25)
  saveres=widget_button(savepan, value='Save', event_pro='drip_eventhand', $
	uvalue={object:self, method:'saveproducts'}, sensitive=0, $
	font=mediumfont, ysize=25)
  autobase=widget_base(savepanall, /row, /nonexclusive, $
	event_pro='drip_eventhand', uvalue={object:self, method:'scan'})
  autosave=widget_button(autobase, value='Auto-Save', event_pro= $
          'drip_eventhand', uvalue={object:self, method:'autosave'}, $
          font=mediumfont)
	    
  ; update pipestat
  (*self.pipestat).resetpipe = new
  (*self.pipestat).open = open
  (*self.pipestat).removefile = cancelopen
  (*self.pipestat).reduce = reduce
  (*self.pipestat).stepback = stepback
  (*self.pipestat).stepbacklist = stepbacklist
  
  (*self.pipeconfstat).confbut = pipeconfidbut
  
  (*self.removeconf).but = cancelopen

  (*self.saveaction).autobut = autosave
  (*self.saveaction).savebut = saveres 
 
  (*self.saveconfstat).but  = savec
  
  ; update button states according to previous buttons
  self->setpipepanel,'same'
  if (*self.saveaction).autosav eq 0 then begin
    widget_control,(*self.saveaction).savebut,sensitive=1  
    widget_control,autosave,set_button=0
  endif else begin
    widget_control,(*self.saveaction).savebut,sensitive=0
    widget_control,autosave,set_button=1
  endelse 
  
  self.maxview = 1
END

;******************************************************************************
;     START - Makes widgets
;             Parameters: requires menu bar and on-screen base widget id
;******************************************************************************

pro drip_pipeman::start, mbar, ctrlbase1, disp_sels=disp_sels
;** make interactive widgets
; get fonts
COMMON gui_os_dependent_values, largefont, smallfont
common gui_config_info, guiconf
; pipeline menu options
pipe_menu=widget_button(mbar, value='PipeLine', /menu)
new=widget_button(pipe_menu, value='New Pipe', event_pro='drip_eventhand', $
      uvalue={object:self, method:'new'}, /separator )
openf=widget_button(pipe_menu, value='Open Forcast File(s) ...', $
      event_pro='drip_eventhand', uvalue={object:self, method:'open'})
openmenu=widget_button(pipe_menu, value='Open a Set of Observations', $
      event_pro='drip_eventhand', uvalue={object:self, method:'opengroup'})
remove=widget_button(pipe_menu, value='Remove Opened File(s) ...', $
      event_pro='drip_eventhand', uvalue={object:self, method:'removeopen'})
reduce=widget_button(pipe_menu, value='Reduce', event_pro='drip_eventhand', $
      uvalue={object:self, method:'run'})
back=widget_button(pipe_menu, value='Undo Image', event_pro='drip_eventhand', $
      uvalue={object:self, method:'stepback'})
rered=widget_button(pipe_menu, value='Re-Reduce (last file)', event_pro='drip_eventhand', $
      uvalue={object:self, method:'re_reduce'})
reredlist=widget_button(pipe_menu, value='Re-Reduce (last list of files)', event_pro='drip_eventhand', $
      uvalue={object:self, method:'re_reduce'})
widget_control,reredlist, sensitive=0
save=widget_button(pipe_menu, value='Save...', event_pro='drip_eventhand', $
      uvalue={object:self, method:'save'} )
saveconf=widget_button(pipe_menu, value='Save Configuration', event_pro='drip_eventhand', $
      uvalue={object:self, method:'saveconf'} )
; configuration and setup options
pipeconfid=widget_button(pipe_menu, value='Pipeline Configuration', $
      event_pro='drip_eventhand', uvalue={object:self, method:'pipeconf'}, $
      /separator )
keywordrep=widget_button(pipe_menu, value='Edit Drip Config...', $
      event_pro='drip_eventhand', $
      uvalue={object:self, method:'dripconf_edit'} )
viewhead=widget_button(pipe_menu, value='View FITS header...', /menu)
viewhead_sub=widget_button(viewhead, value='Base Header', $
      event_pro='drip_eventhand', uvalue={object:self, method:'viewhead'} )
viewhead_sub=widget_button(viewhead, value='Last Header', $
      event_pro='drip_eventhand', uvalue={object:self, method:'viewhead'} )
dispconfid=widget_button(pipe_menu, value='Pipe -> Display Config', $
      event_pro='drip_eventhand', uvalue={object:self, method:'dispconf'} )

self.mainbase = widget_base(ctrlbase1,/row)
panelsbase = widget_base(self.mainbase,/column)
self.deletebase = panelsbase

;---------
; on-screen pipeline control buttons
pipepan = widget_base(panelsbase,/row, /frame)
minbut = widget_button(pipepan,value="<",$
                              event_pro='drip_eventhand',$
                              uvalue={object:self, method:'minimize' })
lab=widget_label(pipepan, value=" Pipeline Control : ", font=largefont) 
newbase = widget_base(pipepan,/column)
new=widget_button(newbase, value='Reset Pipe', event_pro='drip_eventhand', $
      uvalue={object:self, method:'new'}, /sensitive, $
      font=mediumfont, ysize=25)
; configuration and setup options
pipeconfidbut=widget_button(newbase, value='Pipe Conf', $
      event_pro='drip_eventhand', uvalue={object:self, method:'pipeconf'})
filespan = widget_base(pipepan,/column,/frame)
fileslabel = widget_label(filespan,value='File(s)')      
filesbutpan = widget_base(filespan,/row)
open=widget_button(filesbutpan, value='Open', $
      event_pro='drip_eventhand', $
      uvalue={object:self, method:'opengroup'}, /sensitive, $
      font=mediumfont, ysize=25)
cancelopen=widget_button(filesbutpan, value='Remove', $
      event_pro='drip_eventhand', $
      uvalue={object:self, method:'removeopen'}, /sensitive, $
      font=mediumfont, ysize=25)
;test=widget_button(filespan, value='Test', $
;      event_pro='drip_eventhand', $
;      uvalue={object:self, method:'test'}, /sensitive, $
;      font=mediumfont, ysize=25)
redbutpan = widget_base(pipepan,/column)
reduce=widget_button(redbutpan, value='Reduce', event_pro='drip_eventhand', $
      uvalue={object:self, method:'run'}, /sensitive, $
      font=mediumfont, ysize=25)    
stepback=widget_button(redbutpan, value='Undo Image', event_pro='drip_eventhand', $
      uvalue={object:self, method:'stepback'}, /sensitive, $
      font=mediumfont, ysize=25)  
stepbacklist=widget_button(redbutpan, value='Undo List', event_pro='drip_eventhand', $
      uvalue={object:self, method:'stepback'}, /sensitive, $
      font=mediumfont, ysize=25)

*self.pipestat={ $
      stat:0, $             ; Status: See SETPIPEPANEL method for definition
      numred:0, $           ; Number of reduction steps (= number of times we call run method)
      didstepback:0, $         ; (0) if not previous step back, (1) if step back 
      open:open,  $             ; Button for opening files
      openmenu:openmenu,  $     ; Button for opening files in the menu bar
      resetpipe:new, $          ; Button for reseting pipeline
      removefile:cancelopen, $  ; Button for removing files already loaded
      reduce:reduce, $          ; Button for reducing files
      stepback:stepback, $       ; Button for steping back on the reduction
      stepbacklist:stepbacklist $       ; Button for steping back a list on the reduction
      }
      
*self.removeconf={stat:0, $                  ; Status: 0 for window closed, 1 if window open
      but:cancelopen,     $                  ; Button pressed to remove data
      menu:remove,        $                  ; Button pressed in the menu to remove data
      removelist:ptr_new(/allocate_heap),  $ ; Widget ids for remove files
      filesel:ptr_new(/allocate_heap),     $ ; Currently selected files for removal
      allbut:0L,                           $ ; Select all button
      allsel:0,                            $ ; Keep track if all elements are selected
      draw:0L,   $                           ; Define display for image selection
      dispobj:obj_new(),   $                 ; Define the display object
      dispman:obj_new(),   $                 ; Define the display manager
      analscale:obj_new(),   $               ; Define the scale of the selected image
      xysize: 256,                        $ ; Size of display
      butdraw:ptr_new(/allocate_heap),  $    ; Widget ids for image display selection
      done:0L, cancel:0L}            ; widget ids for done and cancel buttons
      
self->setpipepanel,'reset'
;---------
savepanall = widget_base(panelsbase,/row)
savepan = widget_base(savepanall,/row, /frame)
labsav=widget_label(savepan, value=" Data Saving Control: ", font=largefont)
savec=widget_button(savepan, value='Step Definition', event_pro='drip_eventhand', $
      uvalue={object:self, method:'saveconf'}, /sensitive, $
      font=mediumfont, ysize=25)
saveres=widget_button(savepan, value='Save', event_pro='drip_eventhand', $
      uvalue={object:self, method:'saveproducts'}, sensitive=0, $
      font=mediumfont, ysize=25)
autobase=widget_base(savepanall, /row, /nonexclusive, $
      event_pro='drip_eventhand', uvalue={object:self, method:'scan'})
autosave=widget_button(autobase, value='Auto-Save', event_pro= $
        'drip_eventhand', uvalue={object:self, method:'autosave'}, $
        font=mediumfont)
widget_control,autosave,set_button=1

*self.saveaction={autosav:1,          $ ; Is the autosave on?
                  autobut:autosave,   $ ; Auto save button
		  savebut:saveres       $ ; Save button
		 }

;** setup save configuration
*self.saveconfstat={stat:0,            $ ; Status: 0 for window closed, 1 if window open
      calval:ptr_new(/allocate_heap),  $ ; currently selected values for calibration products
      calsel:ptr_new(/allocate_heap),             $ ; widget ids for selectors
      callist:ptr_new(/allocate_heap),            $ ; See above
      ; Not used any more: caldir:'',                    $ ; Name of calibration directory
      caliddir:0.,                  $ ; widget id for calibration directory label
      calselbut:0.,                 $ ; widget id for calibration directory button
      calmakecube:0,                $ ; 1 if we want to create a cube with all the data
      calcubeid:0.,                 $ ; widget id for selecting making a cube
      calallid:0.,                  $ ; widget id for selecting all calibration steps
      calallsel:0,                  $ ; is the all button pressed?
      redval:ptr_new(/allocate_heap),             $ ; currently selected values for reduced products
      redsel:ptr_new(/allocate_heap),             $ ; widget ids for selectors
      reducedlist:ptr_new(/allocate_heap),        $ ; See above
      ; Not used any more: reddir:'',                    $ ; Name of reduced directory
      rediddir:0.,                  $ ; widget id for reduced directory label
      redselbut:0.,                 $ ; widget id for reduced directory button
      redmakecube:0,                $ ; 1 if we want to create a cube with all the data
      redcubeid:0.,                 $ ; widget id for selecting making a cube
      redallid:0.,                  $ ; widget id for selecting all reduced steps
      redallsel:0,                  $ ; is the all button pressed?
      makecube:0,                $ ; 1 if we want to create a cube with all the data
      cubeid:0.,                 $ ; widget id for selecting making a cube
      menu:saveconf,                $ ; widget id for selection in conf menu
      but:savec,                    $ ; widget id for selection in conf button
      done:0L, cancel:0L}            ; widget ids for done and cancel buttons

; Initialize the configuration steps assuming the following steps
; These should match the one in drip__define::INIT
reducedlist = ["CLEANED","DROOPED","IMGLINEARIZED","LINEARIZED","FLATTED","STACKED","UNDISTORTED","MERGED","COADDED","COADDED_ROT"]
callist = ["BADMAP","DARKS","FLATS","LINEAR_CORRECTION","CLEANED_DARKS","DARKSUM","CLEANED_FLATS","DROOPED_FLATS","IMGLINEARIZED_FLATS","MASTERFLAT"]

self.savepath=getpar(guiconf,'savepath')
self.savecalpath=getpar(guiconf,'savecalpath')

ncal = n_elements(callist)
*((*(self.saveconfstat)).callist) = strarr(ncal)
*((*(self.saveconfstat)).calsel) = lonarr(ncal)
*((*(self.saveconfstat)).calval) = replicate(1,ncal)
(*(self.saveconfstat)).calmakecube = 1
for i=0,ncal-1 do begin
  (*((*(self.saveconfstat)).callist))[i] = callist[i]
endfor

nred = n_elements(reducedlist)
*((*(self.saveconfstat)).reducedlist) = strarr(nred)
*((*(self.saveconfstat)).redsel) = lonarr(nred)
*((*(self.saveconfstat)).redval) = replicate(1,nred)
(*(self.saveconfstat)).redmakecube = 1
for i=0,nred-1 do begin
  (*(*(self.saveconfstat)).reducedlist)[i] = reducedlist[i]
endfor
	    
(*(self.saveconfstat)).makecube = 0

self.maxview = 1

auxmsg = ["************ IMPORTANT INFORMATION ****************", $
          "",  $
          "You will be able to change this configuration later", $
          "by going to menu bar -> Pipeline -> Save Configuration", $
          "or pressing the **Step Definition** button in the main window", $
          "",  $
	  "************ IMPORTANT INFORMATION ****************", $
          "",  $
	  "               AUTO-SAVEe is ON                    ", $
          "Remember to turn it off if you do not want the data", $
          "to be saved automatically during the reduction"]
self->saveconf, {id:savec, top:1, handler:savec, select:1}, extramsg=auxmsg
;auxmsg = ["                  AUTO-SAVEe is ON", $
;          "",  $
;          "Remember to turn it off if you do not want the data", $
;          "to be saved automatically during the reduction"]
;res=dialog_message(auxmsg, /INFORMATION, DIALOG_PARENT=autosave)


;** setup pipe config status
; list of keyword options and labels for jbclean (JBC)
jbclist=['FFT','MEDIAN','N']
jbclabel=['Use FFT','Use Median Filter','No Jailbar Clean']
jbclen=n_elements(jbclist)
; list of keyword options and labels for cormerge (CM)
cmlist=['COR','CENT','N']
cmlabel=['Cross Correlation','Centroid','Chop/Nod Data Only']
cmlen=n_elements(cmlist)
cclist=['COR','CENT','N']
cclabel=['Cross Correlation','Centroid','Dither Data Only']
cclen=n_elements(cclist)
spexlist=['OPTIMAL', 'FULLAP']
spexlabel=['Optimal', 'FullAp']
spexlen=n_elements(spexlist)
; setup pipeconfstat
*self.pipeconfstat={stat:0, $ ; Status: 0 for window closed, 1 if window open
      jbcval:'', $ ; currently selected value for Jail Bar Cleaning
      jbcsel:lonarr(jbclen), $ ; widget ids for selectors
      jbclist:jbclist, jbclabel:jbclabel, $ ; See above
      cmval:'', $ ; currently selected value for Corelation Merge
      cmsel:lonarr(cmlen), $ ; widget ids for selectors
      cmlist:cmlist, cmlabel:cmlabel, $ ; See above
      ccval:'', $ ; currently selected value for Corelation Coadd
      ccsel:lonarr(cclen), $ ; widget ids for selectors
      cclist:cclist, cclabel:cclabel, $ ; See above
      confmenu:pipeconfid, $   ; widget id for selection in pipe menu
      confbut:pipeconfidbut, $ ; widget id for selection in pipe button
      done:0L, cancel:0L, $ ; widget ids for done and cancel buttons
      spexval:'',$ ;Currently selected value for spectral extraction mode.
      spexsel:lonarr(spexlen),$ ;widget ids for selectors
      spexlist:spexlist, spexlabel:spexlabel $;See above
     }

auxmsg = ["************ IMPORTANT INFORMATION ****************", $
          "You will be able to change this configuration later", $
          "by going to menu bar -> Pipeline -> Pipeline Configuration"]
self->pipeconf, {id:pipeconfid, top:1, handler:pipeconfid, select:1}, extramsg=auxmsg

; Define structure required for selecting group of images
*self.groupconf={stat:0, $                    ; Status: 0 for window closed, 1 if window open
      top:0L, $                               ; Top widget
      topxpos:500,  $                         ; X position of the popup window
      topypos:300,  $                         ; Y position of the popup window
      grouplist:ptr_new(/allocate_heap),  $   ; Widget ids for groups
      groupsel:0,     $                       ; Currently selected group
      listbase:0L,    $                       ; Widget base containing the file list
      filelist:ptr_new(/allocate_heap),  $    ; Widget ids for files
      fileval:ptr_new(/allocate_heap),   $    ; Contain 1 if file is selected and 0 if not
      filebase:0L,    $                       ; Base containing groups and files
      newbut:0L,  $                           ; Button used to open new directory
      refreshbut:0L, $                        ; Button used to refresh the list of files in the same directory
      doallbut:0L, $                          ; Button added for Basic Science to reduce all groups and save appropriately
      dosetbut:0L, $                          ; Button added for Basic Science to reduce selected groups and save appropriately
      savelist:lonarr(3),  $                  ; Check box to select what products should be saved
      saveval: intarr(3), $                   ; Values correspoding to the selection of products to be saved
      savego: 0L,  $                          ; Button to go ahead and save the selected products
      savecancel: 0L, $                       ; Button to cancel the save operation and remove all products
      savebase: 0L,  $                        ; Base widget to hold save button and messages
      savecleanbase: 0L, $                    ; This will be under savebase and will hold the widgets and be removed when the status change
      done:0L, cancel:0L}                     ; widget ids for done and cancel buttons
  

;** get displays information - setup display configuration status
if keyword_set(disp_sels) then begin
    ; get displays
    *self.disp_sels=disp_sels
    dispn=size(disp_sels,/n_elements)
    ; set dispnewpipe / load from guiconfig
    *self.dispnewpipe=strarr(dispn)
    for dispi=0, dispn-1 do begin
        paramname='disp_pipe_pref_'+strtrim(string(dispi),2)
        pref=getpar(guiconf,paramname)
        if size(pref,/type) eq 7 then (*self.dispnewpipe)[dispi]=pref $
        else (*self.dispnewpipe)[dispi]='None'
    endfor
    ; make display configuration status
    *self.dispconfstat={stat:0, dispdrop:lonarr(dispn), dispsel:intarr(dispn),$
                        menu:dispconfid, done:0L, cancel:0L}
endif else begin
    ; no displays available - set status to 0
    *self.dispconfstat={stat:0, menu:dispconfid}
endelse
end

;******************************************************************************
;     CLEANUP - Free pointer heap variables, save settings
;******************************************************************************

pro drip_pipeman::cleanup
;** save configuration info
common gui_config_info, guiconf
; Save Paths
setpar,guiconf,'loaddatapath',self.loaddatapath
setpar,guiconf,'savepath',self.savepath
setpar,guiconf,'savecalpath',self.savecalpath
; save dispnewpipe steps
dispn=size(*self.dispnewpipe,/n_elements)
for dispi=0,dispn-1 do begin
    paramname='disp_pipe_pref_'+strtrim(string(dispi),2)
    setpar,guiconf,paramname,(*self.dispnewpipe)[dispi]
endfor
;** free pointers
ptr_free, self.disp_sels
ptr_free, self.filelist
ptr_free, self.loadfilter
ptr_free, self.saveconfstat
ptr_free, self.saveaction
ptr_free, self.pipestat
ptr_free, self.groupconf
ptr_free, self.removeconf
ptr_free, self.pipeconfstat
ptr_free, self.dispnewpipe
ptr_free, self.dispconfstat
if obj_valid(self.drip) then obj_destroy, self.drip
if obj_valid(self.objgroup) then obj_destroy, self.objgroup
; Destroy mw object
obj_destroy, self.mw
end

;******************************************************************************
;     INIT - Initialize structure
;******************************************************************************

function drip_pipeman::init, dataman, mw
; assign objects
self.dataman=dataman
self.mw=mw
self.disp_sels=ptr_new(/allocate_heap)
; reset file list
self.filelist=ptr_new(/allocate_heap)
*self.filelist=['']
; set load filter
self.loadfilter=ptr_new(/allocate_heap)
*self.loadfilter=['*.fits','*.fit','*.fts']
; get loaddatapath, savepath and savecalpath
; (check if entries are valid and paths exist)
common gui_config_info, guiconf
loaddatapath=getpar(guiconf,'loaddatapath')
if size(loaddatapath,/type) ne 7 then self.loaddatapath='.' else begin
    if file_test(loaddatapath) gt 0 then $
      self.loaddatapath=loaddatapath else self.loaddatapath='.'
endelse
savepath=getpar(guiconf,'savepath')
if size(savepath,/type) ne 7 then self.savepath='' else begin
    if file_test(savepath) gt 0 then $
      self.savepath=savepath else self.savepath='.'
endelse
savecalpath=getpar(guiconf,'savecalpath')
if size(savecalpath,/type) ne 7 then self.savecalpath='' else begin
    if file_test(savecalpath) gt 0 then $
      self.savecalpath=savecalpath else self.savecalpath=''
endelse
; get save config popup status memory
self.saveconfstat=ptr_new(/allocate_heap)
self.saveaction=ptr_new(/allocate_heap)
; get pipe config popup status memory
self.pipestat=ptr_new(/allocate_heap)
self.groupconf=ptr_new(/allocate_heap)
self.removeconf=ptr_new(/allocate_heap)
self.pipeconfstat=ptr_new(/allocate_heap)
; get dispnewpipe memory
self.dispnewpipe=ptr_new(/allocate_heap)
; get display config popup status memory
self.dispconfstat=ptr_new(/allocate_heap)
; set saveflag = 1 (to avoid dialog if no data is present)
self.saveflag=1
return, 1
end

;******************************************************************************
;     DRIP_PIPEMAN__DEFINE
;******************************************************************************

pro drip_pipeman__define

struct={drip_pipeman, $
        ; other objects
        mw:obj_new(), $       ; message window object
        mainbase: 0L,   $        ; widget_base containing the widget_base to be deleted
	maxview:0,        $      ; boolean indicating if the view is maximized
        deletebase: 0L,   $      ; widget_base containing under the mainbase 
        dataman:obj_new(), $  ; data manager
        disp_sels:ptr_new(), $; array of display anal select objects
        ; loading and saving information
        loaddatapath:'', $    ; loading directory for forcast data
        loadfilter:ptr_new(),$; filters for loading images
        lastfile:'',$         ; last data file loaded
        savepath:'', $        ; saving reduced data directory
        savecalpath:'', $        ; saving calibration data directory
        saveflag:0,$          ; flag to check if a reduced data is saved
	; Group of objects
	objgroup:obj_new(), $ ; It is used to classify images according to their headers
        ; pipe variables: data reduction
        drip:obj_new(), $     ; data reduction pipe object
        mode:'', $            ; instrument mode of the current pipeline
        ; files to process
        n:0, $                ; the number of entries in filelist
        ind:0, $              ; the index of next file to reduce in filelist
        filelist:ptr_new(), $ ; file list for current pipe
                              ; (all files: reduced, stepedback, non-reduced)
        ; reduced pipeline data
        cnt:0, $              ; number of summed files for current pipe
                              ; (can be !=ind b/c of stepback)
        name:'', $            ; name of current pipe dap
                              ; !! This is how dap is found in dataman !!
        next:0, $             ; number of next pipe
        ; pipeline setup
        pipestat:ptr_new(),$     ; record buttons for sensitivity status
        groupconf:ptr_new(),$     ; record buttons for sensitivity status during group selection
        removeconf:ptr_new(),$     ; record buttons for sensitivity status
        pipeconfstat:ptr_new(),$ ; record variable with status of popup window
                              ; with pipeline configuration
        ; saving setup
        saveconfstat:ptr_new(),$ ; record variable with status of popup window
                              ; with save configuration
        saveaction:ptr_new(),$ ; record variable with status of save buttons
        ; pipeline displays interaction
        dispnewpipe:ptr_new(),$; string array with name of new pipestep
                               ; to be displayed for each display
                               ; 'none' for not requested
        dispconfstat:ptr_new() } ; record variable with  status of
                               ; popup window with display configuration
end
