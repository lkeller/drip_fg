function fdrp_getdata,filelist,HEADERS=outhdrs,INPUTFILE=infile,DATASEC=datasec

;+
; NAME:
;       FDRP_GETDATA
; 
; PURPOSE:
;       Data retrieval element.  Takes a list of FITS files and creates IDL 
;       image and header arrays.  This procedure assumes ONE image per file.
;
; CALLING SEQUENCE:
;       IMAGES = GETDATA(FILELIST,HEADERS=outhdrs,/INPUTFILE,DATASEC=[x0,x1,y0,y1]
;
; INPUTS:
;       FILELIST - string array of FITS filenames OR name of file containing 
;       			list of FITS files to read (with keyword /INPUTFILE set).
;
; OUTPUT:
;       IMAGES - 	IDL integer array containing all the images from the input FITS
;       			files in a data cube, array(*,*,n), where n is the number of 
;       			image files.  
;       HEADERS - 	IDL string array containing all the FITS headers associated with
;       			the input files, strarr(*,n), where n is the number of files.
;
; OPTIONAL INPUT KEYWORDS:
;       /INPUTFILE - 	If /INPUTFILE is set, FILELIST is assumed to be a text file 
;       			containing a list of FITS files to be read - one per line.
;	   DATASEC - Pass a 4 element vector corresponding to the good section of the chip.
;	   			This section is cut out and processed, the remainder is ignored.
;	   			For example, to denote the upper half of the chip:
;	   			DATASEC=[0,1023,514,1023].
;
; VERSION HISTORY:
;       	6-30-2003 -- RYS: First good version, converted to a function.
;			10-30-2002 - RYS:  Added datasection capability
;			8-9-2002 - RYS: First cut at things.
;       			
;-

on_error,1

print,'-----------------------------------'
print,'GETDATA:  STARTED...'

if (n_params() NE 1) then begin
	print,'Calling Sequence: ims = GETDATA(FILELIST,HEADERS=outhdrs,DATASEC=[x0,x1,y0,y1],/INPUTFILE'
	return,0
endif

;If INPUTFILE keyword is set, then read a list of files out of a text file
if keyword_set(infile) then begin
	readcol,filelist,temp,format='A'  ;read files into temp string array file...
	filelist=temp
endif


;read first file to get initial setup info...
print,'GETDATA:  Initializing image arrays...'
n=n_elements(filelist)
rawim=readfits(filelist[0],hdr)

if keyword_set(datasec) then dataim=rawim[datasec[0]:datasec[1],datasec[2]:datasec[3]] $
	else dataim=rawim

imsize=size(dataim)

;initialize output array/headers
outims=fltarr(imsize[1],imsize[2],n)
outhdrs=strarr(500,n)

print,'GETDATA:  Reading FITS files...'
for i=0,n-1 do begin
	rawim=readfits(filelist[i],hdr)
	if keyword_set(datasec) then temp=rawim[datasec[0]:datasec[1],datasec[2]:datasec[3]] $
		else temp=rawim
	
	outims[*,*,i]=temp		

	outhdrs[0,i]=hdr[*]
endfor

print,'GETDATA:  ...COMPLETE'
print,'-----------------------------------'

return,outims

end



;---------------------------------------------------


