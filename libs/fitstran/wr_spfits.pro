PRO wr_spfits,fits_file,sp_data,len_array,FITS_HDR=fits_hdr,COLLABELS=collabels,QUIET=quiet

; authors:  G.C. Sloan and M.J. Russell
;
; version 2.2

;  9 Oct 04 2.2 GCS - slight change - note that active version is elsewhere
; 23 Jul 04 2.2 GCS - debugging yesterday's changes
; 22 Jul 04 2.2 GCS - improved error handling, removed stops in program
;                     if '/' in file name, print error message, don't write file
;                     if total(len_array) < 0 then load len_array here
;                       orders of zero length skipped
;                     number of col labels no longer has to match number of cols
;                     added quiet keyword to suppress warnings
; 22 Jan 04 2.1 MJR - add COLLABELS keyword (touched up 27 Jan 04)
; 20 Jan 04 1.2 modifying FITS header with default comment for Col 4
;  3 Dec 03 1.2 expand the number of possible columns
; 10 Oct 03 1.1 delete EXTEND keyword in original header
;  3 Oct 03 1.1 allow len_array to be have one value of zero
; 15 Jul 03 1.0 adapted from sws2fits.pro
;  6 Apr 03     created as sws2fits.pro
;
; generates a spectral fits file in n-column format
; preferred column order is:  wavelength, flux, flux_error, 
;   segment/order/aperture, flag
; but columns can be anything 
; this format is designed for segmented spectra, which can have overlaps
;   between segments
;
; INPUT:
;   fits_file - the name of the FITS file to write - required
;   sp_data -   the data in 2-plus-column format - required
;   len_array - an array with the length of each spectral segment - required
;               the sum of the segment lengths must equal the no. of rows
;               in sp_data
;               if total(len_array)=0, then NSEG set to 1
;               if total(len_array)<0, then len_array constructed here
;   fits_hdr -  optional - the starting FITS header, which will be modified
;               the FITS header should include a DATE label
;               if not provided, a fresh FITS header will be generated
;   collabels - optional - a string array with the label for each data column
;               the number of labels no longer has to match the number of cols
;   quiet     - optional keyword to suppress warnings (but not errors)
; OUTPUT:       writes FITS file to disk, nothing returned to calling procedure

; check keywords and set flags and generic variables

if (keyword_set(quiet) eq 0) then quietflag=0 else quietflag=1
if (keyword_set(collabels) eq 0) then labelflag=0 else labelflag=1
errflag=0
errtext='Error in sm_wr_spfits.'
warntext='Warning in sm_wr_spfits.'

; check for two-dimensional data with at least two columns

sz_data=size(sp_data)
ndim=sz_data[0]
ncol=sz_data[1]
if (ndim ge 2) then nrow=sz_data[2]
if (ndim ne 2) then begin
  errflag=1
  print,errtext+'  Spectral data array must have two dimensions.'
endif
if (ncol lt 2) then begin
  errflag=1
  print,errtext+'  Spectral data must have two or more columns.'
endif

; if total(len_array)<0, then create length array

if (total(len_array) lt 0) then begin

;   determine order column (ocol) or assume default (3)

  ocol=-1
  if (labelflag eq 1) then begin
    for i=0,n_elements(collabels)-1 do begin
      if (collabels[i] eq 'order') then ocol=i
      if (collabels[i] eq 'segment') then ocol=i
      if (collabels[i] eq 'aperture') then ocol=i
    endfor
  endif
  if (ocol eq -1) then begin
    if (quietflag eq 0) then $
      print,warntext+'  Setting order column to default (3)'
    ocol=3            ; default order column is column 3
  endif

;   redefine len_array

  ord_cnt=0
  minorder=min(sp_data[ocol,*])
  maxorder=max(sp_data[ocol,*])
  for m=minorder,maxorder do begin
    index=where(sp_data[ocol,*] eq m)
    if (max(index) gt -1) then ord_cnt=ord_cnt+1
  endfor
  len_array=intarr(ord_cnt)
    
;   load len_array

  ord_idx=0
  for m=minorder,maxorder do begin
    index=where(sp_data[ocol,*] eq m)
    if (max(index) gt -1) then begin
      len_array[ord_idx]=n_elements(index)
      ord_idx=ord_idx+1
    endif
  endfor

endif

; if total(len_array)=0, then set NSEG=1 and NSEG01 = length of entire array

if (total(len_array) eq 0) then len_array=nrow

; check to see if total(len_array) = no. of rows in sp_data

if (total(len_array) ne nrow) then begin
  errflag=1
  print,errtext+'  Total of length array incorrect.'
endif

; copy sp_data into out_data, creating an error column (of 0) if necessary

if (ncol eq 2) then begin
  out_data=dblarr(3,nrow)
  out_data[0:1,*]=sp_data
endif else begin
  out_data=dblarr(ncol,nrow)
  out_data=sp_data
endelse

; generate header or copy fits_hdr input at command line

if (n_elements(fits_hdr) eq 0) then mkhdr,h0,out_data $
else h0=fits_hdr

; add fresh FITS keywords to header
; add column labels to header first (so they appear below NSEG keywords)

if (n_elements(collabels) eq 0) then begin
  if (quietflag eq 0) then $
    print,warntext+'  No columns labels added to FITS header.'
endif else begin

  if (n_elements(collabels) ne ncol) then begin
    colstop=min([n_elements(collabels),ncol])
    if (quietflag eq 0) then print,warntext,'  Not all columns labelled.'
  endif else colstop=ncol

  for i=0,colstop-1 do begin
    if (i lt 9) then val = string(format='("COL0",i1)',i+1) $
    else             val = string(format='("COL",i2)',i+1)
    val = val + 'DEF'
    comment=string(format='(" Label for data in column",i2)',i+1)
    sxaddpar,h0,val,collabels(i),comment,before='DATE   '
  endfor

endelse

; nseg keywords second (so they appear above column labels)

nseg=n_elements(len_array)
sxaddpar,h0,'NSEG',nseg,' Number of spectral segments',before='DATE   '

for i=0,nseg-1 do begin
  if (i lt 9) then label=string(format='("NSEG0",i1)',i+1) $
  else             label=string(format='("NSEG",i2)',i+1)
  comment=string(format='(" Length of segment ",i2)',i+1)
  sxaddpar,h0,label,len_array[i],comment,before='DATE   '
endfor

; old code to add column labels as comments - no longer used
;  sxaddpar,h0,'COMMENT','Col. 4 = order number','',after='DATE   '
;  sxaddpar,h0,'COMMENT','Col. 3 = uncertainty in flux (Jy)','',after='DATE   '
;  sxaddpar,h0,'COMMENT','Col. 2 = flux (Jy)','',after='DATE   '
;  sxaddpar,h0,'COMMENT','Col. 1 = wavelength (micron)','',after='DATE   '

; remove keywords which might be left from YAAAR or similar formats

sxdelpar,h0,'EXTEND '

; check for '/' in file name, print error message if found

;if (stregex(fits_file, '/', /fold_case) ne -1) then begin
;  print,errtext+'  Slash in filename.'
;  errflag=1
;endif

; if errflag not set, then write spectral FITS file, else print file
; name
;print,'fits_file: ',fits_file
;help,out_data
;print,'h0: ', h0

if (errflag eq 0) then writefits,fits_file,out_data,h0
if (errflag eq 1) then print,'File '+fits_file+' not written.'

END
