function MakeNoise, x,y,std,mean
;x,y is the size of the image , std is the standard deviation used in the  
;Gaussian distibution and the mean is where the data is centered
array=Randomn(seed,x,y)
array=array*std
array=array+mean
return,array
end 




