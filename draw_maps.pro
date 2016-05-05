pro draw_maps,region=region,Lvrange=Lvrange

;if ~keyword_set(region) then region='BFS52'
;if ~keyword_set(Lvrange) then Lvrange=[5,11]
;if ~keyword_set(region) then region='GGMC1'
;if ~keyword_set(Lvrange) then Lvrange=[2,9]
;if ~keyword_set(region) then region='GGMC2'
;if ~keyword_set(Lvrange) then Lvrange=[5,11]
;if ~keyword_set(region) then region='GGMC3'
;if ~keyword_set(Lvrange) then Lvrange=[5,11]
;if ~keyword_set(region) then region='GGMC4'
;if ~keyword_set(Lvrange) then Lvrange=[-3,5]
;if ~keyword_set(region) then region='lynds'
;if ~keyword_set(Lvrange) then Lvrange=[-3,3]
if ~keyword_set(region) then region='west'
if ~keyword_set(Lvrange) then Lvrange=[-1,4]
;if ~keyword_set(region) then region='swallow'
;if ~keyword_set(Lvrange) then Lvrange=[12,18]
;if ~keyword_set(region) then region='horn'
;if ~keyword_set(Lvrange) then Lvrange=[12,18]
;if ~keyword_set(region) then region='remote'
;if ~keyword_set(Lvrange) then Lvrange=[18,28]

cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/'+region+'/candidates'


;fits_read,'../Lpeakv0.fits',dat,hdr
;
;dathdr=list(dat,hdr)
;
;crp1=sxpar(hdr,'CRPIX1')
;crv1=sxpar(hdr,'CRVAL1')
;del1=sxpar(hdr,'CDELT1')
;l_l=(360.+(0.5-crp1)*del1+crv1) mod 360
;l_r=(360.+(sxpar(hdr,'NAXIS1')+0.5-crp1)*del1+crv1) mod 360
;   
;crp2=sxpar(hdr,'CRPIX2')
;crv2=sxpar(hdr,'CRVAL2')
;del2=sxpar(hdr,'CDELT2')
;b_d=(0.5-crp2)*del2+crv2
;b_u=(sxpar(hdr,'NAXIS2')+0.5-crp2)*del2+crv2
;
;x_range=[l_l,l_r]
;y_range=[b_d,b_u]
;ratio=abs((b_u-b_d)/(l_l-l_r))
;
;psname='Lpeakv.eps'
;cgps_open,psname,font=!p.font,/quiet,default_thickness=1.0,charsize=1.25,/portrait
;xsize=1100. & ysize=1000.*ratio
;cgDisplay, xsize=round(xsize), ysize=round(ysize)
;pos0=[150./xsize,100./ysize,1.-100./xsize,1-50./ysize]
;pos1=[1-100./xsize,100./ysize,1.-70./xsize,1-50./ysize]
;cgplot,[0],[0],xrange=x_range,yrange=y_range,$
;  xtickinterval=0.25,ytickinterval=0.25,aspect=ratio,AxisColor='black',$
;  ytitle='Galactic Latitude (!Uo!N)',xtitle=textoidl('Galactic Longitude (^{o})'),position=pos0
;cgloadct,39,clip=[48,255];,ncolors=208
;dat[where(finite(dat) eq 0)]=max(dat,/nan)+0.00001
;cgimage,bytscl(dat,/nan),/overplot;,/noerase,position=pos0
;cgColorbar,position=pos1,yMINOR=5,/vertical,ncolors=254,/right,range=[min(dat,/nan),max(dat,/nan)]
;
;cgplot,[0],[0],xrange=x_range,yrange=y_range,position=pos0,$
;  xtickinterval=0.25,ytickinterval=0.25,aspect=ratio,$
;  AxisColor='black',/noerase,xtickformat='(a1)',ytickformat='(a1)'
;
;cgps_close
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;fits_read,'../Urvmap.fits',datr,hdrr
;
;crp1=sxpar(hdrr,'CRPIX1')
;crv1=sxpar(hdrr,'CRVAL1')
;del1=sxpar(hdrr,'CDELT1')
;l_l=(360.+(0.5-crp1)*del1+crv1) mod 360
;l_r=(360.+(sxpar(hdrr,'NAXIS1')+0.5-crp1)*del1+crv1) mod 360
;   
;crp2=sxpar(hdrr,'CRPIX2')
;crv2=sxpar(hdrr,'CRVAL2')
;del2=sxpar(hdrr,'CDELT2')
;b_d=(0.5-crp2)*del2+crv2
;b_u=(sxpar(hdrr,'NAXIS2')+0.5-crp2)*del2+crv2
;
;x_range=[l_l,l_r]
;y_range=[b_d,b_u]
;ratio=abs((b_u-b_d)/(l_l-l_r))
;
;psname='Urvmap.eps'
;cgps_open,psname,font=!p.font,/quiet,default_thickness=1.0,charsize=1.25,/portrait
;xsize=1100. & ysize=1000.*ratio
;cgDisplay, xsize=round(xsize), ysize=round(ysize)
;pos0=[150./xsize,100./ysize,1.-100./xsize,1-50./ysize]
;pos1=[1-100./xsize,100./ysize,1.-70./xsize,1-50./ysize]
;cgplot,[0],[0],xrange=x_range,yrange=y_range,$
;  xtickinterval=0.25,ytickinterval=0.25,aspect=ratio,AxisColor='black',$
;  ytitle='Galactic Latitude (!Uo!N)',xtitle=textoidl('Galactic Longitude (^{o})'),position=pos0
;cgloadct,56,clip=[0,223];,ncolors=22,/reverse
;;dat[where(datr eq 0)]=max(dat,/nan)+0.00001
;cgimage,bytscl(datr),/overplot;,/noerase,position=pos0
;
;readcol,'../redpeaks.txt',num,peak1,peak2, cen1,cen2,size1,sizw2,wing,format='I,F,F,F,F,F,F,F',stringskip='#'
;for i=0, n_elements(num)-1 do begin
;  cgplot,peak1[i],peak2[i],/over,psym=cgsymcat(1,thick=3),color='white';,psymthick=5
;  cgplot,peak1[i],peak2[i],/over,psym=cgsymcat(1),color='red'
;endfor
;
;cgColorbar,position=pos1,yMINOR=10,/vertical,/right,range=[min(datr),max(datr)]
;
;cgplot,[0],[0],xrange=x_range,yrange=y_range,position=pos0,$
;  xtickinterval=0.25,ytickinterval=0.25,aspect=ratio,$
;  AxisColor='black',/noerase,xtickformat='(a1)',ytickformat='(a1)'
;
;cgps_close

fits_read,'../'+region+'_La_mask.fits',datLa,hdrLa

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

szratio=ratio*3.5

psname='f_map_'+region+'.eps'
cgps_open,psname,font=!p.font,/quiet,default_thickness=1.0,charsize=0.7,/portrait
xsize=1000.+250 & ysize=1000.*ratio+150
cgDisplay, xsize=round(xsize), ysize=round(ysize)
pos0=[150./xsize,100./ysize,1.-100./xsize,1-50./ysize]
pos1=[1-100./xsize,100./ysize,1.-100./xsize+30*ratio/xsize,1-50./ysize]
cgplot,[0],[0],xrange=x_range,yrange=y_range,$
  xticklen=0.01,yticklen=0.015*ratio,$
  ;xtickinterval=round((max(x_range)-min(x_range))*10*ratio)/50d,ytickinterval=round((max(y_range)-min(y_range))*10)/50d,$
  xtickinterval=0.25*(1+(l_l-l_r ge 2)),ytickinterval=0.25*(1+(l_l-l_r ge 2)),$
  aspect=ratio,AxisColor='black',$
  ytitle='Galactic Latitude (!Uo!N)',xtitle=textoidl('Galactic Longitude (^{o})'),position=pos0

cropfits,dathdrL,Lvrange,dim='v',/dataform,output=crL
Liimap=total(smooth(crL.dat,[1,1,3]),3)*abs(double(sxpar(crl.hdr,'CDELT3')))/1000.
Liimap[where(Liimap lt 0)]=0
numc=round(max(Liimap))
;print,numc
levels=median(liimap)>sqrt((Lvrange[1]-Lvrange[0])*0.16)+(indgen(round(sqrt(numc)))^2)
cgloadct,0,ncolors=numc,/reverse
;cgcontour,Liimap,/onimage,nlevels=numc,label=0,c_colors=indgen(numc),/fill
cgimage,bytscl(Liimap),/overplot,ncolors=numc
cgcontour,Liimap,/onimage,levels=levels,label=0,color='green',thick=0.2
cgColorbar,position=pos1,yMINOR=5,/vertical,ncolors=numc,/right,range=[min(Liimap),max(Liimap)]

cgplot,[0],[0],xrange=x_range,yrange=y_range,position=pos0,$
  xticklen=0.01,yticklen=0.015*ratio,$
  ;xtickinterval=round((max(x_range)-min(x_range))*10*ratio)/50d,ytickinterval=round((max(y_range)-min(y_range))*10)/50d,$
  xtickinterval=0.25*(1+(l_l-l_r ge 2)),ytickinterval=0.25*(1+(l_l-l_r ge 2)),$
  aspect=ratio,AxisColor='black',/noerase,xtickformat='(a1)',ytickformat='(a1)'

readcol,'blue_out.cat',bsn,bpeak1,bpeak2,bw0,bw1,bnote,format='I,F,F,F,F,A',stringskip='#'
readcol,'red_out.cat',rsn,rpeak1,rpeak2,rw0,rw1,rnote,format='I,F,F,F,F,A',stringskip='#'
readcol,'outflowcat.cat',num,glon,glat,bluev0,bluev1,redv0,redv1,tag,bnum,rnum,format='I,F,F,F,F,F,F,A,I,I',stringskip='#'

for i=0,n_elements(num)-1 do begin
  if tag[i] eq 'D' then begin
    j=where(bsn eq bnum[i])
    k=where(rsn eq rnum[i])
    cgplot,/over,bpeak1[j],bpeak2[j],psym=cgsymcat(9,thick=0.7),symsize=3*szratio/(l_l-l_r),symcolor='blue';,thick=3
    cgplot,/over,rpeak1[k],rpeak2[k],psym=cgsymcat(9,thick=0.7),symsize=3*szratio/(l_l-l_r),symcolor='red'
    cgtext,glon[i],glat[i]-0.00375*(b_u-b_d),color='white',/data,num2str(num[i]),charsize=1.2*szratio/(l_l-l_r),alignment=0.5,charthick=3*ratio
    cgtext,glon[i],glat[i]-0.00375*(b_u-b_d),color=cgcolor('purple'),/data,num2str(num[i]),charsize=1.2*szratio/(l_l-l_r),alignment=0.5,charthick=1*ratio
  endif
  if tag[i] eq 'B' then begin
    j=where(bsn eq bnum[i])
    cgplot,/over,bpeak1[j],bpeak2[j],psym=cgsymcat(9,thick=0.7),symsize=3*szratio/(l_l-l_r),symcolor='blue'
    cgtext,glon[i],glat[i]-0.00375*(b_u-b_d),color='white',/data,num2str(num[i]),charsize=1.2*szratio/(l_l-l_r),alignment=0.5,charthick=3*ratio
    cgtext,glon[i],glat[i]-0.00375*(b_u-b_d),color='blue',/data,num2str(num[i]),charsize=1.2*szratio/(l_l-l_r),alignment=0.5,charthick=1*ratio
  endif
  if tag[i] eq 'R' then begin
    k=where(rsn eq rnum[i])
    cgplot,/over,rpeak1[k],rpeak2[k],psym=cgsymcat(9,thick=0.7),symsize=3*szratio/(l_l-l_r),symcolor='red'
    cgtext,glon[i],glat[i]-0.00375*(b_u-b_d),color='white',/data,num2str(num[i]),charsize=1.2*szratio/(l_l-l_r),alignment=0.5,charthick=3*ratio
    cgtext,glon[i],glat[i]-0.00375*(b_u-b_d),color='red',/data,num2str(num[i]),charsize=1.2*szratio/(l_l-l_r),alignment=0.5,charthick=1*ratio
  endif
endfor


if file_test('/home/Alpha/Astrodata/galaxysurvey/-18_30/mask_gem/'+region) then begin
  readcol,'/home/Alpha/Astrodata/galaxysurvey/-18_30/mask_gem/'+region,gl,gb,stringskip='#',/silent
  cgplot,gl,gb,/over,linestyle=1
endif

cgps_close

end