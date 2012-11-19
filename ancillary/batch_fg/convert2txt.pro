pro convert2txt,infile

path='/Users/lkeller/Documents/Writing_Projects/papers/HaebeII/sloan_redux/haebespec/fits/'
a=readfits(path+infile)
idx=sort(a[0,*])
b=a[*,idx]
openw,1,path+infile+'.txt'
printf,1,b[0:2,*]
close,/all
end

