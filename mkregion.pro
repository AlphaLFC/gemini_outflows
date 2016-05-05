pro mkregion

cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/out_search_scripts/'


fits_read,'../mosaic_U.fits',datUall,hdrUall
fits_read,'../mosaic_L.fits',datLall,hdrLall
fits_read,'../mosaic_L2.fits',datL2all,hdrL2all
dathdrU=list(datUall,hdrUall)
dathdrL=list(datLall,hdrLall)
dathdrL2=list(datL2all,hdrL2all)

readcol,'region.txt',name,lrange0,lrange1,brange0,brange1,vLrange0,vLrange1,vL2range0,vL2range1,$
  stringskip='#',/silent,format='A,F,F,F,F,F,F,F,F'

idx=n_elements(name)

for i=0, idx-1 do begin
  
  cropfits,/dataform,dathdrU,[lrange0[i],lrange1[i]],[brange0[i],brange1[i]],[vLrange0[i]-10,vLrange1[i]+10],output=cUa
  cropfits,/dataform,dathdrL,[lrange0[i],lrange1[i]],[brange0[i],brange1[i]],[vLrange0[i]-10,vLrange1[i]+10],output=cLa
  cropfits,/dataform,dathdrL2,[lrange0[i],lrange1[i]],[brange0[i],brange1[i]],[vLrange0[i]-10,vLrange1[i]+10],output=cL2a
  fits_write,name[i]+'_Ua_C',cUa.dat,cUa.hdr
  fits_write,name[i]+'_La_C',cLa.dat,cLa.hdr
  fits_write,name[i]+'_L2a_C',cL2a.dat,cL2a.hdr
  
  if ~file_test(name[i]) then spawn,'mkdir '+name[i]
  spawn,'mv '+name[i]+'_*_C.fits '+name[i]
    
endfor


end