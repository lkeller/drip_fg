; NAME:
;     DRIP_DATAMAN - Version 1.7.0
;
; PURPOSE:
;     Data manager to store and serve all data displayed in the GUI
;
; DEFINITIONS:
;     DAP = DAta Products
;           IDL structure with any format. One element, caled 'name' is
;           is required.
;
; CALLING SEQUENCE / INPUTS / OUTPUTS: NA
;
; CALLED ROUTINES AND OBJECTS:
;   The DRIP_DATAMAN does not use any other objects.
;   DRIP_MENU and DRIP_PIPEMAN: change DAP data and issue channelcall
;                               commands
;   DRIP_ANAL_SELECT: Uses DRIP_DATAMAN for sending data to the displays.
;
; PROCEDURE:
;   Dataman stores all the GUI data as DAPs in an array of structures.
;   Other GUI objects set and get DAPs and DAP elements using the
;   dataman member functions. The dataman also stores a list of all
;   the GUI objetcs that need to be notifed (channelcall) if any data
;   has been changed. It is also possible to use and access the dataman
;   from the IDL command line. Finally, the dataman is part of the GUI
;   and it's widgets allows the user to view dap info, edit DAP
;   names and delete DAPs.
;
; RESTRICTIONS:
;     Use dapnew, get/setdap, get/set/checkelement to avoid inconsistencies
;
; MODIFICATION HISTORY:
;     Written by:  Alfred Lee, Cornell University, 2002
;     Modified:   Alfred Lee, Cornell University, January 2003
;                 Decided to give this version a number
;     Modified:   Alfred Lee, CU, January 30, 2003
;                 Handles display events differently.  discovered bugs fixed.
;     Modified:   Marc Berthoud, CU, July 2003
;                 Passed choice of pipeline step selection into
;                 DISP windows
;     Modified:   Marc Berthoud, CU, May 2004
;                 Now accesses the displays through disp_channels
;     Modified:   Marc Berthoud, CU, July 2004
;                 Adapted to new pipe
;     Modified:   Marc Berthoud, CU, April 2006
;                 - renamed dispman -> dataman
;     Rewritten:  Marc Berthoud, Palomar, July 2006
;                 - Introduced DAta Products for information storage
;                   that contain elements
;                 - Use of flexible channels for data update
;                   notification
;                 - Moved all pipeline interaction to pipeman
;     Modified:   Marc Berthoud, CU, August 2007
;                 - Added printhelp and printdaps user commands
;                 - Moved widgets creation from gui->dataman
;                 - Added killdap function and [[delete]] button

;******************************************************************************
;     RESET - Reset daps
;******************************************************************************
pro drip_dataman::reset
; free memory
for dapi=0,self.dapn-1 do begin
    ptr_free,self.daps[dapi]
endfor
; reset counter
self.dapn=0
self.dapsel=''
; call channels
self->channelcall
; reset dap info
*self.dap_infoval= $
	[['Object:','','Obs Mode:',''], ['Nr. of Files:','','Last File:',''], $
	 ['FC Channel:','','FilterCenter:',''], ['Date:','','Time:','']]
if self.maxview eq 1 then $
  widget_control, self.dap_info, set_value=*self.dap_infoval
end

;******************************************************************************
;     SETDATA - Adjust the SELF structure elements
;******************************************************************************

pro drip_dataman::setdata, dap_infoval=dap_info, dapsel=dapsel
if (self.maxview eq 0) then return
if keyword_set(dap_info) then self.dap_info=dap_info
if keyword_set(dapsel) then self.dapsel=dapsel
end

;******************************************************************************
;     GETDATA - Returns the SELF structure elements
;******************************************************************************

function drip_dataman::getdata, dap_infoval=dap_info, dapn=dapn, daps=daps
if (self.maxview eq 0) then return,-1
if keyword_set(dap_info) then return, self.dap_info
if keyword_set(dapn) then return, self.dapn
if keyword_set(daps) then return, self.daps
end

;******************************************************************************
;     GETDAP - Get Pointer to DAP with dapname, returns null pointer if no dap
;              with dapname available (test with pointer_valid())
;******************************************************************************
function drip_dataman::getdap,dapname
; return null if no daps available
if self.dapn eq 0 then return, ptr_new()
; check if dap with dapname exits
dapi=-1
repeat begin
    dapi=dapi+1
    ; get name
    name=(*self.daps[dapi]).name
endrep until (dapi eq self.dapn-1) or (dapname eq name)
; return dap pointer or new dap pointer
if dapname eq name then begin
    return, self.daps[dapi]
endif else begin
    return, ptr_new()
endelse
end

;******************************************************************************
;     SETDAP - Overwrite DAP with newdata (creates new one if not
;              available, adds name tag if not available)
;              newdata needs to be a structure
;******************************************************************************
pro drip_dataman::setdap,dapname, newdata
; check if there is a 'name' tag
elemn=n_tags(newdata)
elems=[tag_names(newdata),''] ; add '' for saftey
elemi=0
while (elemi lt elemn) and (elems[elemi] ne 'NAME') do elemi=elemi+1
; add name tag if necessary
; if name exists force overwrite with new name
if elemi eq elemn then newdata=create_struct('NAME',dapname, newdata) else $
  newdata.name=dapname
; check if dap with dapname exists
if self.dapn gt 0 then begin
    dapi=-1
    repeat begin
        dapi=dapi+1
        ; get name
        name=(*self.daps[dapi]).name
    endrep until (dapi eq self.dapn-1) or (dapname eq name)
endif else name=''

; make new dap if necessary

; self.daps is a fixed-length pointer array; we need to check
; if there is room in the array
nmax = n_elements(self.daps)
if dapname ne name then begin

   if self.dapn ge nmax then begin
      drip_message, 'dataman::setdap: Too many data sets (>'+$
                    strtrim(nmax,2) + ').', /fatal
      return
   endif

    self.daps[self.dapn]=ptr_new(/allocate_heap)
    dapi=self.dapn
    self.dapn=self.dapn+1
endif
; set new dap data
*(self.daps[dapi])=newdata
end

;******************************************************************************
;     KILLDAP - Erases a DAP (can be pointer or name)
;******************************************************************************
pro drip_dataman::killdap,dap
; check if any daps available
if self.dapn eq 0 then return
; get dap index
if ptr_valid(dap) then begin
    dapi=-1
    repeat begin
        dapi=dapi+1
    endrep until (dapi eq self.dapn-1) or (dap eq self.daps[dapi])
    if dap ne self.daps[dapi] then return
endif else begin
    ; check if dap is a string
    if size(dap,/type) ne 7 then return
    dapname=dap
    ; get dap pointer
    dapi=-1
    repeat begin
        dapi=dapi+1
        ; get name
        name=(*self.daps[dapi]).name
    endrep until (dapi eq self.dapn-1) or (dapname eq name)
    if dapname ne name then return
endelse
; copy all following daps
while dapi lt self.dapn-1 do begin
    *self.daps[dapi]=*self.daps[dapi+1]
    dapi=dapi+1
endwhile
; clear last dap memory
ptr_free,self.daps[dapi]
self.dapn=self.dapn-1
end

;******************************************************************************
;     LISTDAP - Writes list of DAP names into list, Returns number of DAPs
;******************************************************************************
function drip_dataman::listdap,namelist
; if no DAPs available
if self.dapn eq 0 then begin
    namelist=['']
    return,0
endif else begin
    ; put names of daps in namelist
    for dapi=0,self.dapn-1 do begin
        ; get name
        name=(*(self.daps[dapi])).name
        if dapi gt 0 then namelist=[namelist,name] else namelist=[name]
    endfor
    ; return number of daps
    return, self.dapn
endelse
end

;******************************************************************************
;     GETELEMENT - Return an Element in a DAP (dap can be pointer or
;                  name). Returns -1 if element is not found (or dap invalid).
;                  ****** Use checkelement if a -1 value ********
;                         crashes your code
;******************************************************************************
function drip_dataman::getelement,dap,element
; check if any daps available
if self.dapn eq 0 then return, -1
; get dap pointer if dap is a dapname
if ptr_valid(dap) eq 0 then begin
    ; check if dap is a string
    if size(dap,/type) ne 7 then return, -1
    dapname=dap
    ; get dap pointer
    dapi=-1
    repeat begin
        dapi=dapi+1
        ; get name
        name=(*self.daps[dapi]).name
    endrep until (dapi eq self.dapn-1) or (dapname eq name)
    if dapname eq name then dapptr=self.daps[dapi] else return, -1
endif else dapptr=dap
; check if element is valid
if (size(element,/type)ne 7) or (size(element,/dimensions)gt 0) then return,-1
; search element
elemn=n_tags(*dapptr)
elems=[tag_names(*dapptr),'']
elemi=0
while (elemi lt elemn) and (elems[elemi] ne element) do elemi=elemi+1
; return element
if elemi eq elemn then return,-1 else return, (*dapptr).(elemi)
end

;******************************************************************************
;     CHECKELEMENT - Return an Info about a DAP Element. Returns 1 if
;                    element is available, 0 otherwise. With size set
;                    function returns  the size of the element (or
;                    [0,0,0]). Dap can be pointer or dapname
;******************************************************************************
function drip_dataman::checkelement, dap, element, size=size
; set failure return value
if keyword_set(size) then retval=[0,0,0] else retval=0
; check if any daps available
if self.dapn eq 0 then return, retval
; get dap pointer if dap is not a pointer
if ptr_valid(dap) eq 0 then begin
    ; check if dap is a string
    if size(dap,/type) ne 7 then return, retval
    dapname=dap
    ; get dap pointer
    dapi=-1
    repeat begin
        dapi=dapi+1
        ; get name
        name=(*self.daps[dapi]).name
    endrep until (dapi eq self.dapn-1) or (dapname eq name)
    if dapname eq name then dapptr=self.daps[dapi] else return, retval
endif else dapptr=dap
; check if element is valid
if (size(element,/type)ne 7) or (size(element,/dimensions)gt 0) then $
  return, retval
; search element
elemn=n_tags(*dapptr)
elems=[tag_names(*dapptr),'']
elemi=0
while (elemi lt elemn) and (elems[elemi] ne element) do elemi=elemi+1
; return check
if elemi eq elemn then return, retval else begin
    if keyword_set(size) then return,size((*dapptr).(elemi)) else return,1
    return,1
endelse
end

;******************************************************************************
;     SETELEMENT - Sets / Creates an Element in a DAP and fills it
;                  with data. Dap can be pointer or dapname.
;******************************************************************************
pro drip_dataman::setelement, dap, element, data
; abort if no daps available
if self.dapn eq 0 then return
; get dap pointer if dap is not a pointer
if ptr_valid(dap) eq 0 then begin
    ; check if dap is a string
    if size(dap,/type) ne 7 then return
    dapname=dap
    ; get dap pointer
    dapi=-1
    repeat begin
        dapi=dapi+1
        ; get name
        name=(*self.daps[dapi]).name
    endrep until (dapi eq self.dapn-1) or (dapname eq name)
    if dapname eq name then dapptr=self.daps[dapi] else return
endif else dapptr=dap
; check if element is valid
if (size(element,/type)ne 7) or (size(element,/dimensions)gt 0) then return
; search element
elemn=n_tags(*dapptr)
elems=[tag_names(*dapptr),'']
elemi=0
while (elemi lt elemn) and (elems[elemi] ne element) do elemi=elemi+1
; set element
if elemi lt elemn then begin
    ; compare size of old and new element
    sold=size((*dapptr).(elemi))
    snew=size(data)
    if array_equal(sold,snew) then (*dapptr).(elemi)=data else begin
        ; *** this complicated procedure is necessay to cover the case that
        ;     the size of the element changes (ex: longer/shorter array)
        ; make structure before element to replace
        ; (can just copy first element as name wouldn't change in size)
        begstruct=create_struct(elems[0],(*dapptr).(0))
        for i=1,elemi-1 do $
          begstruct=create_struct(begstruct, elems[i],(*dapptr).(i))
        ; check if element is last in structure
        if elemi eq elemn-1 then begin
            *dapptr=create_struct(begstruct,element,data)
        endif else begin
            ; make structure after element to replace
            endstruct=create_struct(elems[elemi+1],(*dapptr).(elemi+1))
            for i=elemi+2,elemn-1 do $
              endstruct=create_struct(endstruct, elems[i], (*dapptr).(i))
            *dapptr=create_struct(begstruct,element,data,endstruct)
        endelse
    endelse
endif else *dapptr=create_struct(*dapptr,element,data)
end

;******************************************************************************
;     LISTELEMENT - Returns number and list of elements in a DAP
;******************************************************************************
function drip_dataman::listelement, dap, list
; make fake list in case error is detected
list=['listelement_error']
if self.dapn eq 0 then return, 0
; get dap pointer if dap is not a pointer
if ptr_valid(dap) eq 0 then begin
    ; check if dap is a string
    if size(dap,/type) ne 7 then return, 0
    dapname=dap
    ; get dap pointer
    dapi=-1
    repeat begin
        dapi=dapi+1
        ; get name
        name=(*self.daps[dapi]).name
    endrep until (dapi eq self.dapn-1) or (dapname eq name)
    if dapname eq name then dapptr=self.daps[dapi] else return, 0
endif else dapptr=dap
; return list
list=tag_names(*dapptr)
return,n_tags(*dapptr)
end

;******************************************************************************
;     CHANNELADD - Adds an Object to be Notified if the DAPs change
;                  An object stays added until it is destroyed
;******************************************************************************
pro drip_dataman::channeladd, object
if size(object,/type) eq 11 then begin
    self.channels[self.channeln]=object
    self.channeln=self.channeln+1
endif
end

;******************************************************************************
;     CHANNELCALL - Notifies all objects that DAPs have changed
;                   Checks if every object is valid and cuts invalid
;                   ones from list
;******************************************************************************
pro drip_dataman::channelcall
; only run if channelcall is not already running
if self.channelcalling eq 0 then begin
    ; set channelcalling
    self.channelcalling=1
    ;** set list of DAPs in dap_select widget
    ; get list for dap_select
    dapn=self->listdap(dapnames)
    dapnames=[dapnames,'']
    ; IF dapn > 0 (i.e. daps present)
    if dapn gt 0 then begin
	;** notify channels
	objcp=0 ; where next object to copy (only necessary if some are invalid)
	for obji=0, self.channeln-1 do begin
            if obj_valid(self.channels[obji]) then begin
        	; notify object
        	(self.channels[obji])->newdap
        	; if objects have been invalid, move stack back
        	if objcp lt obji then $
        	  self.channels[objcp]=self.channels[obji]
        	objcp=objcp+1
            endif
	endfor
	; shorten the object list for invalid objects
	self.channeln=objcp+1
        ; get index of currently selected dap
        dapi=-1
        repeat dapi=dapi+1 $
          until (dapi eq dapn) or (dapnames[dapi] eq self.dapsel)
        if dapi eq dapn then begin
            dapi=0
            self.dapsel=dapnames[0]
        endif
        ; set widgets
	if (self.maxview eq 1) then begin
	  widget_control, self.dap_select, set_droplist_select=dapi, $
            set_value=dapnames[0:dapn-1], sensitive=1
          widget_control, self.dap_rename, sensitive=1
          widget_control, self.dap_delete, sensitive=1
	endif
    ; ELSE ( dapn == 0 i.e. no daps )
    endif else begin
        ; set widgets
	if (self.maxview eq 1) then begin
          widget_control, self.dap_select, set_droplist_select=0, $
            set_value=['No Data Products'], sensitive=0
          widget_control, self.dap_rename, sensitive=0
          widget_control, self.dap_delete, sensitive=0
	endif
    endelse
    ; call info
    self->callinfo
    ; reset channelcalling
    self.channelcalling=0
endif
end

;******************************************************************************
;     CALLINFO - updates info message
;******************************************************************************
pro drip_dataman::callinfo, dapsel=dapsel
;** make dap_info message
COMMON gui_os_dependent_values, largefont, smallfont
; if dapsel is set, copy the value
if keyword_set(dapsel) then self.dapsel=dapsel
; get DAP
if self.dapn gt 0 then begin
    ; get dap (pointer) and dapind
    dapn=self->listdap(dapnames)
    dapind=(where(dapnames eq self.dapsel))[0]
    dap=self->getdap(self.dapsel)
    ; if dap valid:
    if ptr_valid(dap) then begin
        ; if it's a pipe dap
        if self->checkelement(dap,'MODE') gt 0 then begin
            header=self->getelement(dap,'HEADER')
            (*self.dap_infoval)[1,0]=sxpar(header,'OBJECT')
            mode=strtrim(self->getelement(dap,'MODE'),2)
            if mode eq 'TEST' then begin
                mode=mode+' '+strtrim(sxpar(header,'TESTMRGE'),2)
                mode=mode+' '+strtrim(sxpar(header,'TESTCADD'),2)
            endif
            (*self.dap_infoval)[3,0]=mode
            (*self.dap_infoval)[1,1]=string(self->getelement(dap,'N'))
            filename=self->getelement(dap,'FILENAME')
            (*self.dap_infoval)[3,1]=STRMID(filename, $
                STRPOS(filename, path_sep(), /REVERSE_SEARCH)+1)
            (*self.dap_infoval)[1,2]=sxpar(header,'DETECTOR')
            (*self.dap_infoval)[3,2]=string(sxpar(header,'WAVELNTH'))
            (*self.dap_infoval)[1,3]=sxpar(header,'DATE-OBS')
            (*self.dap_infoval)[3,3]=sxpar(header,'TIME-OBS')
        ; else (i.e. not a pipe dap)
        endif else begin
            ; fill general information (n, filename)
            if self->checkelement(dap,'N') then $
              (*self.dap_infoval)[1,1]=string(self->getelement(dap,'N')) else $
              (*self.dap_infoval)[1,1]=''
            if self->checkelement(dap,'FILE0') then $
              (*self.dap_infoval)[3,1]=self->getelement(dap,'FILE0') else $
              (*self.dap_infoval)[3,1]=''
            ; fill information from header (date, time)
            header=-1
            if self->checkelement(dap,'HEADER') then $
              header=self->getelement(dap,'HEADER')
            if self->checkelement(dap,'HEADER0') then $
              header=self->getelement(dap,'HEADER0')
            if size(header,/N_DIMENSIONS) then begin
                (*self.dap_infoval)[1,2]=sxpar(header,'DETECTOR')
                (*self.dap_infoval)[3,2]=string(sxpar(header,'WAVELNTH'))
                (*self.dap_infoval)[1,3]=sxpar(header,'DATE-OBS')
                (*self.dap_infoval)[3,3]=sxpar(header,'TIME-OBS')
            endif else begin
                (*self.dap_infoval)[1,2]=''
                (*self.dap_infoval)[3,2]=''
                (*self.dap_infoval)[1,3]=''
                (*self.dap_infoval)[3,3]=''
            endelse
            ; fill last fields
            (*self.dap_infoval)[1,0]=''
            (*self.dap_infoval)[3,0]=''
        endelse
    endif
endif else begin
    ; no valid files -> fill all with spaces
    for i=0,3 do for j=0,1 do (*self.dap_infoval)[j*2+1,i]=''
    ;for i=0,7 do (*self.dap_infoval)[1,i]=''
    dapind=0
endelse
; update widget text
if (self.maxview eq 0) then return
widget_control, self.dap_info, set_value=*self.dap_infoval
widget_control, self.dap_select, set_droplist_select=dapind
end

;******************************************************************************
;     PRINTDAP - prints list of all daps with all elements
;       (for command line use)
;       Sample Output:
;       DAP0=mathstuff
;         ELEM0=name[Scalar/String]="mathstuff"
;         ELEM1=n[Scalar/Integer]=5
;         ELEM2=arr[2D(2,2)/Float]:med=1.0 mean=0.5 min=0.0 max=1.0
;******************************************************************************
pro drip_dataman::printdap

;** Loop through daps
for dapi=0, self.dapn-1 do begin
    ;** Print name
    ; get pointer and name
    dapptr=self.daps[dapi]
    dapname=self->getelement(dapptr,'NAME')
    ; print it
    print,dapname
    ;** Get list and number of elements
    elemn=self->listelement(dapptr,elemlist)
    ;** Loop through Elements
    for elemi=0, elemn-1 do begin
        ; get element value
        elemval=(*dapptr).(elemi)
        ; get element size -> make size string
        elemsiz=size(elemval)
        elemdim=elemsiz[0]
        elemtype=elemsiz[elemdim+1]
        if elemdim eq 0 then sizestr='(Scalar/' else begin
            sizestr='('+strtrim(string(elemsiz[0]),2)+'D['
            sizestr=sizestr+strtrim(string(elemsiz[1]),2)
            for dimi=2, elemdim do $
              sizestr=sizestr+','+strtrim(string(elemsiz[dimi]),2)
            sizestr=sizestr+']/'
        endelse
        switch elemtype of
            0: begin
                sizestr=sizestr+'Undef)'
                break
            end
            1:
            2:
            3:
            12:
            13:
            14:
            15:begin
                sizestr=sizestr+'Integer)'
                elemtype=1
                break
            end
            4:
            5: begin
                sizestr=sizestr+'Float)'
                elemtype=4
                break
            end
            6:
            9: begin
                sizestr=sizestr+'Complex)'
                elemtype=6
                break
            end
            7: begin
                sizestr=sizestr+'String)'
                break
            end
            8: begin
                sizestr=sizestr+'Struct)'
                break
            end
            10: begin
                sizestr=sizestr+'Ptr)'
                break
            end
            11: begin
                sizestr=sizestr+'ObjRef)'
                break
            end
        endswitch
        ; get value string
        if (elemtype gt 0) and (elemtype lt 7) then begin
            if elemdim eq 0 then valstr='='+strtrim(string(elemval),2) $
            else valstr=': med='+strtrim(string(median(elemval)),2) $
              +' mean='+strtrim(string(mean(elemval)),2) $
              +' min='+strtrim(string(min(elemval)),2) $
              +' max='+strtrim(string(max(elemval)),2)
        endif else if elemtype eq 7 then begin
            if elemdim eq 0 then valstr='='+elemval $
            else valstr='='+elemval[0]
        endif
        ; print element string
        print,'  '+elemlist[elemi]+sizestr+valstr
    endfor
endfor
end

;******************************************************************************
;     PRINTHELP - prints list of all commands (for command line use)
;******************************************************************************
pro drip_dataman::printhelp

print,"DRIP_GUI Data Manager: User Commands list"
print,"General:"
print,"  - Data is strored in DAta Packages (DAP)"
print,"  - Each DAP is a named structure,"
print,"    it can contain any number of elements of any type"
print,"Acessing DAPs:"
print,"  n=dataman->listdap(list) : get dap names"
print,"  pointer=dataman->getdap(dapname) : returns pointer to dap structure"
print,"  dataman->setdap,dapname,dap_struct : create / overwrite a dap"
print,"  dataman->killdap,dapname : erase a dap"
print,"Acessing DAP elements:"
print,"  n=dataman->listelement(dapname,list) : get element names"
print,"  data=dataman->getelement(dapname,elementname) : get element value"
print,"  dataman->setelement(dapname,elementname,data) : set element value"
print,"  res=dataman->checkelement(dapname,element) : check if element exists"
print,"Support:"
print,"  dataman->printhelp : prints data manager interactive commands"
print,"  dataman->printdap : prints list of all daps with elements"
print,"GUI Interaction:"
print,"  dataman->channelcall : informs DRIP_GUI that new data is in dataman"
print,"    **!! if using GUI call this after using setdap or setelement !!**"
end

;******************************************************************************
;     INPUT - user input handler for widgets
;******************************************************************************
pro drip_dataman::input, event
case event.id of
    ; select DAP
    self.dap_select: begin
        dapn=self->listdap(dapnames)
        self.dapsel=dapnames[event.index]
        self->callinfo
    end
    ; rename current DAP
    self.dap_rename: begin
        newname=dialog_input('Enter New Name for '+self.dapsel)
        if strlen(newname) gt 0 then begin
            dap=self->getdap(self.dapsel)
            (*dap).name=newname
            self->channelcall
        endif
    end
    ; delete current DAP
    self.dap_delete: begin
        self->killdap,self.dapsel
        self->channelcall
    end
endcase
end

;******************************************************************************
;     MINIMIZE - Minimize the panel containing all the widgets
;******************************************************************************
pro drip_dataman::minimize, event
  widget_control,self.deletebase,/destroy
  
  self.deletebase = widget_base(self.mainbase, /row, /frame)
  minbut = widget_button(self.deletebase,value=">",$
                        	event_pro='drip_eventhand',$
                        	uvalue={object:self, method:'maximize' })
  lab=widget_label(self.deletebase, value="Click to expand information of data in focus",font=smallfont)
  self.maxview = 0
end

;******************************************************************************
;     MAXIMIZE - Minimize the panel containing all the widgets
;******************************************************************************
pro drip_dataman::maximize, event
  widget_control,self.deletebase,/destroy
  
  self->start,self.mainbase
  self.channelcalling = 0
  self->channelcall
end

;******************************************************************************
;     START - Creates the widgets to display DAP information
;******************************************************************************

pro drip_dataman::start, cbase0

; get font string from common block
common gui_os_dependent_values, largefont, smallfont
; make widgets
self.mainbase = cbase0
self.deletebase = widget_base(self.mainbase, /row, /frame)
cbase3=widget_base(self.deletebase, /column )

titlebase = widget_base(cbase3,/row)
minbut = widget_button(titlebase,value="<",$
                              event_pro='drip_eventhand',$
                              uvalue={object:self, method:'minimize' })
lab=widget_label(titlebase, value="Data In Focus: ",font=largefont)

cbase4=widget_base(cbase3, /row )
self.dap_delete=widget_button(cbase4, value='Delete', $
                              event_pro='drip_eventhand',$
                              uvalue={object:self, method:'input' }, $
                              sensitive=0 )
self.dap_rename=widget_button(cbase4, value='Rename', $
                              event_pro='drip_eventhand',$
                              uvalue={object:self, method:'input' }, $
                              sensitive=0 )
self.dap_select=widget_droplist(cbase3, value=['  No _ Data _ Products  '], $
                                event_pro='drip_eventhand', $
                                uvalue={object:self, method:'input' }, $
                                font=smallfont, sensitive=0, ysize=45)
self.dap_info=widget_table(self.deletebase, xsize=4, ysize=4, $
                           /no_headers, $
                           column_widths=[85,100,85,100], $
                           value=[['Empty']] )
; fill dap infoval
*self.dap_infoval= $
	[['Object:','','Obs Mode:',''], ['Nr. of Files:','','FileName:',''], $
	 ['FC Channel:','','FilterCenter:',''], ['Date:','','Time:','']]
widget_control, self.dap_info, set_value=*self.dap_infoval

self.maxview = 1
end

;******************************************************************************
;     CLEANUP - Free pointer heap variables
;******************************************************************************

pro drip_dataman::cleanup
; free pointers
for dapi=0,self.dapn-1 do begin
    ptr_free,self.daps[dapi]
endfor
ptr_free, self.dap_infoval
end

;******************************************************************************
;     INIT - Initialize structure
;******************************************************************************

function drip_dataman::init, mw
self.mw=mw ; get mw
; set memory for dap_info values
self.dap_infoval=ptr_new(/allocate_heap)
return, 1
end

;******************************************************************************
;     DRIP_DATAMAN__DEFINE
;******************************************************************************

pro drip_dataman__define

struct={drip_dataman, $
        mainbase: 0L,   $        ; widget_base containing the widget_base to be deleted
        deletebase: 0L,   $      ; widget_base containing under the mainbase 
	                         ; include all the widgets after start is called
	maxview:0,        $      ; boolean indicating if the view is maximized
	mw:obj_new(), $          ;message window object
        ; data analysis products (DAPs)
        dapn:0, $                ; number of DAPs
        daps:ptrarr(200), $       ; pointers to DAPs
        dapsel:'', $             ; name DAP of currently selected
        ; IDs of screen widgets
        dap_info:0L, $           ; widget id of DAP information table
        dap_select:0L, $         ; widget id of DAP selection pulldown
        dap_rename:0L, $         ; widget id of DAP rename button
        dap_delete:0L, $         ; widget id of DAP delete button
        dap_infoval:ptr_new(), $ ; the DAP information table
        ; channels (i.e. other objects) to be called if DAPs change
        channeln:0, $            ; number of channels
        channels:objarr(20), $   ; object references to objects to call
        channelcalling:0}        ; flag set to 1 while calling channels
                                 ; (to avoid nested calls)
end
