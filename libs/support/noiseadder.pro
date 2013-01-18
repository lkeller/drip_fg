function Noiseadder, filepath,std
;creates and adds noise to the four images in one of the data files. filepath is the path of the data and std is the standard diviation used when creating the random noise. I found .025
;has small but noticeable noise.
fakedata=readfits(filepath)
fakedatanoise=fakedata 
for j=0,3 do begin
   noise=makenoise(256,256,std,1)
   fakedatanoise[*,*,j]=fakedata[*,*,j]*noise
   ;j=j+1  THIS gets done AUTOMATICALLY in for loops 
  endfor 
return, fakedatanoise
end
