pro list_header_data, data_path, list_file_name 

fitsdir,filename,keyvalue,data_path,t=data_path+list_file_name, /nosize, $
keywords='OBJECT, WAVELNTH, DICHROIC, ZA_START, ALTI_STA'
  
 ; infile = readfits(fpath[i]+'/'+datfile[i],hdr)
 ;  filtr  = fxpar(hdr,'WAVELNTH')
 ;  za     = (fxpar(hdr,'ZA_START') + fxpar(hdr,'ZA_END'))/2.0
 ;  alt    = (fxpar(hdr,'ALTI_STA') + fxpar(hdr,'ALTI_END'))/2.0
  
end