pro geminiout,inputparafile

;if
region='GGMC1'
lrange=[192.5,195.25]
brange=[-2,0.6]
Uvrange=[-3,10]
Lvrange=[-0.3,10]
L2vrange=[2,9]

if ~file_test('./'+region) then spawn,'mkdir ./'+region
cd,'./'+region

outnameU=region+'_U'
outnameL=region+'_L'
outnameL2=region+'_L2'

if ~file_test(region+'_U_C.fits') then cropfits,'../mosaic_U.fits',lrange,brange,outfile=region+'_U',Uvrange
if ~file_test(region+'_L_C.fits') then cropfits,'../mosaic_L.fits',lrange,brange,outfile=region+'_L',Lvrange
if ~file_test(region+'_L2_C.fits') then cropfits,'../mosaic_L2.fits',lrange,brange,outfile=region+'_L2',L2vrange
end