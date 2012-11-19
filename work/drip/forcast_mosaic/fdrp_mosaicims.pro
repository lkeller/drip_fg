;+
;		ROT_ANGLE = optional array of rotation angles

if not keyword_set(rot_angle) then begin
	rot_angle=exptime_arr
	rot_angle[*]=0.0
endif  


;now rotate all arrays:
;rotate new frame
newframe=rot(newframe,rot_angle[i])
newexp=rot(newexp,rot_angle[i])
newweight=rot(newweight,rot_angle[i])


;sum counts per pixel(sclaed) and multiply by weight to get rid of bad pixels
mosaic=(newframe_expand*newweight_expand)+mosaic_expand