pro manu_check,region=region,Lvrange=Lvrange

;if ~keyword_set(region) then region='BFS52'
;if ~keyword_set(Lvrange) then Lvrange=[5,11]
;if ~keyword_set(region) then region='GGMC1'
;if ~keyword_set(Lvrange) then Lvrange=[2,9]
;if ~keyword_set(region) then region='GGMC2'
;if ~keyword_set(Lvrange) then Lvrange=[5,11]
;if ~keyword_set(region) then region='GGMC3'
;if ~keyword_set(Lvrange) then Lvrange=[5,11]
if ~keyword_set(region) then region='GGMC4'
if ~keyword_set(Lvrange) then Lvrange=[-3,5]
;if ~keyword_set(region) then region='lynds'
;if ~keyword_set(Lvrange) then Lvrange=[-3,3]
;if ~keyword_set(region) then region='west'
;if ~keyword_set(Lvrange) then Lvrange=[-1,4]
;if ~keyword_set(region) then region='swallow'
;if ~keyword_set(Lvrange) then Lvrange=[12,18]
;if ~keyword_set(region) then region='horn'
;if ~keyword_set(Lvrange) then Lvrange=[12,18]
;if ~keyword_set(region) then region='remote'
;if ~keyword_set(Lvrange) then Lvrange=[18,28]

cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/'+region+'/candidates'

char_ratio=1
sz_ratio=1
;fits_read,region+'_Ua_mask.fits',datUa,hdrUa
fits_read,'../'+region+'_La_C.fits',datLa,hdrLa

dathdrL=list(datLa,hdrLa)

crp1=sxpar(hdrLa,'CRPIX1')
crv1=sxpar(hdrLa,'CRVAL1')
del1=sxpar(hdrLa,'CDELT1')
l_l=(360.+(0.5-crp1)*del1+crv1) mod 360
l_r=(360.+(sxpar(hdrLa,'NAXIS1')+0.5-crp1)*del1+crv1) mod 360
   
crp2=sxpar(hdrLa,'CRPIX2')
crv2=sxpar(hdrLa,'CRVAL2')
del2=sxpar(hdrLa,'CDELT2')
b_d=(0.5-crp2)*del2+crv2
b_u=(sxpar(hdrLa,'NAXIS2')+0.5-crp2)*del2+crv2

x_range=[l_l,l_r]
y_range=[b_d,b_u]
ratio=abs((b_u-b_d)/(l_l-l_r))

psname='outmap.eps'
cgps_open,psname,font=!p.font,/quiet,default_thickness=1.0,charsize=char_ratio;,/encapsulated;,/portrait
xsize=1000.*sz_ratio & ysize=1000.*ratio*sz_ratio
cgDisplay, xsize=round(xsize), ysize=round(ysize)
pos0=[100./xsize,100./ysize,1.-50./xsize,1-50./ysize]
cgplot,[0],[0],xrange=x_range,yrange=y_range,$
  xtickinterval=0.5,ytickinterval=0.5,aspect=ratio,AxisColor='black',xthick=5,ythick=5,$
  ytitle='Galactic Latitude (!Uo!N)',xtitle=textoidl('Galactic Longitude (^{o})'),position=pos0
cgloadct,0,/reverse;,ncolors=10

cropfits,dathdrL,Lvrange,dim='v',/dataform,output=crL
Liimap=total(smooth(crL.dat,[1,1,3]),3)
cgimage,bytscl(Liimap<max(liimap)/1.5),/overplot;,/noerase,position=pos0

;cropfits,dathdrL2,Lvrange,dim='v',/dataform,output=crL2
;L2iimap=total(crL2.dat,3)
;L2iimap=smooth(L2iimap,[3,3],/edge_mirror)
;levels=max(L2iimap)*(0.15+indgen(10)*0.1)
;cgcontour,/onimage,L2iimap,label=0,color='red',level=levels
;levels=levels,label=1,color='blue'

cgplot,[0],[0],xrange=x_range,yrange=y_range,position=pos0,$
  xtickinterval=0.5,ytickinterval=0.5,aspect=ratio,$
  AxisColor='black',/noerase,xtickformat='(a1)',ytickformat='(a1)'

readcol,'red_out.cat',nnum,npeak1,npeak2,nwing1,nwing2,rnote,format='I,F,F,F,F,A',/silent
for i=0, n_elements(npeak1)-1 do begin
  ;cgplot,/over,npeak1[i],npeak2[i],psym=9,symsize=0.75,symcolor='white',thick=3
  cgplot,/over,npeak1[i],npeak2[i],psym=9,symsize=0.75*char_ratio,symcolor='red',thick=3
  ;cgplot,/over,npeak1[i],npeak2[i],psym=7,symsize=0.5,symcolor='white',thick=3
  ;cgplot,/over,npeak1[i],npeak2[i],psym=7,symsize=0.5,symcolor='red',thick=1
  cgtext,npeak1[i]-0.03,npeak2[i]+0.015,color='white',/data,num2str(nnum[i]),charsize=0.8*char_ratio,alignment=0.5,charthick=3
  cgtext,npeak1[i]-0.03,npeak2[i]+0.015,color='red',/data,num2str(nnum[i]),charsize=0.8*char_ratio,alignment=0.5,charthick=1
endfor
readcol,'blue_out.cat',bnnum,bnpeak1,bnpeak2,bnwing1,bnwing2,bnote,format='I,F,F,F,F,A',/silent
for i=0, n_elements(bnpeak1)-1 do begin
  ;cgplot,/over,bnpeak1[i],bnpeak2[i],psym=9,symsize=0.75,symcolor='white',thick=3
  cgplot,/over,bnpeak1[i],bnpeak2[i],psym=9,symsize=0.75*char_ratio,symcolor='blue',thick=3
  ;cgplot,/over,bnpeak1[i],bnpeak2[i],psym=1,symsize=0.75,symcolor='white',thick=3
  ;cgplot,/over,bnpeak1[i],bnpeak2[i],psym=1,symsize=0.75,symcolor='blue',thick=1
  cgtext,bnpeak1[i]+0.03,bnpeak2[i]+0.015,color='white',/data,num2str(bnnum[i]),charsize=0.8*char_ratio,alignment=0.5,charthick=3
  cgtext,bnpeak1[i]+0.03,bnpeak2[i]+0.015,color='blue',/data,num2str(bnnum[i]),charsize=0.8*char_ratio,alignment=0.5,charthick=1
endfor

readcol,'outflowcat.txt',num,gl,gb,b0,b1,r0,r1,tag,bn,rn,format='I,F,F,F,F,F,F,A,I,I',stringskip='#',/silent
for i=0, n_elements(num)-1 do begin
  if tag[i] eq 'D' then begin
    ;cgsymcat()
    ;cgplot,/over,gl[i],gb[i],psym=9,symsize=0.8*char_ratio,symcolor='green',thick=3
    j=where(bnnum eq bn[i])
    k=where(nnum eq rn[i])
    dis=sqrt((bnpeak1[j]-npeak1[k])^2+(bnpeak2[j]-npeak2[k])^2)
    major=dis/2+0.02
    if dis ne 0 then begin
      if bnpeak1[j]-npeak1[k] ne 0 then pa=atan((bnpeak2[j]-npeak2[k])/(bnpeak1[j]-npeak1[k])) $
        else pa=atan(100000000)
    endif else pa=0
    cgcolorfill,draw_ellipse(gl[i],gb[i],major,0.005,pa=pa,n_points=12,/rad),Color='green',thick=2;,NOCLIP=0
    cgcolorfill,draw_ellipse(gl[i],gb[i],0.015,0.002,pa=(pa+1.57),n_points=12,/rad),Color='orange',thick=3;,NOCLIP=0
  endif
endfor


if file_test('/home/Alpha/Astrodata/galaxysurvey/-18_30/mask_gem/'+region) then begin
  readcol,'/home/Alpha/Astrodata/galaxysurvey/-18_30/mask_gem/'+region,gl,gb,stringskip='#',/silent
  cgplot,gl,gb,/over,linestyle=1
endif

cgps_close

end