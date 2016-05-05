pro cal_region

regions=['GGMC1','GGMC2','GGMC3','GGMC4','BFS52','lynds','west','swallow','horn','remote']
distances=[2,2,2,2,2,0.4,0.6,3.8,3.8,8.5]

for i=0,9 do begin
  cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/'+regions[i]
  fits_read,regions[i]+'_Ua_C.fits',datUa,hdrUa
  fits_read,regions[i]+'_La_C.fits',datLa,hdrLa
  fits_read,'Lpeakv0.fits',peakvmap,vhdr
  dathdr=list(datUa,hdrUa)
  dathdrL=list(datLa,hdrLa)
endfor