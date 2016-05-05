function wisescl,dat
min=median(dat)-(max(dat)-median(dat))*0.2
max=max(dat)-(max(dat)-median(dat))*0.1
re_dat=reform(logscl(dat,min=min,max=max),1,n_elements(dat[*,0]),n_elements(dat[0,*]))
return, re_dat
end


pro new_outflow;,vres=vres;,area=area

cd,'/home/Alpha/Astrodata/REDUCTION/OUTFLOW'

;readcol,'outflowpara_test',sign1,l,b,v,v_width,v_bin,v1,v2,d,LINE,bv1,bv2,rv1,rv2,area_x,area_y,spec_x,spec_y,/silent,stringskip='#',$
;  format='(A,F,F,F,F,F,F,F,F,A,F,F,F,F,F,F,F,F)'
;readcol,'outflowpara_new',sign1,l,b,v,v_width,v_bin,v1,v2,d,LINE,bv1,bv2,rv1,rv2,area_x,area_y,spec_x,spec_y,/silent,stringskip='#',$
;  format='(A,F,F,F,F,F,F,F,F,A,F,F,F,F,F,F,F,F)'
  
readcol,'outflowpara_new_revised',sign1,l,b,v,v_width,v_bin,v1,v2,d,LINE,bv1,bv2,rv1,rv2,area_x,area_y,spec_x,spec_y,/silent,stringskip='#',$
  format='(A,F,F,F,F,F,F,F,F,A,F,F,F,F,F,F,F,F)'
  
readcol,'outflowpara2',sign2,blue_l,blue_b,red_l,red_b,bszx,bszy,rszx,rszy,/silent,stringskip='#',$
  format='(A,F,F,F,F,F,F,F,F)'

readcol,'outflowlevels',sign3,dblev,blev0,drlev,rlev0,/silent,stringskip='#',format='(A,F,F,F,F)'

;pos=make_array(4,6,/float)
;for m=0,2 do begin 
;  for n=0,1 do begin
;    pos[*,m+3*n]=[0.06+m/3.*0.97,0.06+n/2.*0.97,0.06+(m+1.)/3.*0.97-0.045,0.06+(n+1.)/2.*0.97-0.04]
;  endfor
;endfor

;pos=make_array(4,9,/float)
;for m=0,2 do begin 
;  for n=0,2 do begin
;    pos[*,m+3*n]=[0.06+m/3.*0.97,0.04+n/3.*0.97,0.06+(m+1.)/3.*0.97-0.045,0.04+(n+1.)/3.*0.97-0.02]
;  endfor
;endfor

for nn=0, 2 do begin

psname='f_new_out_'+num2str(nn)+'.eps'
print,'  Figure '+psname+' will be created.'

if nn ne 2 then begin
pos=make_array(4,9,/float)
for m=0,2 do begin 
  for n=0,2 do begin
    pos[*,m+3*n]=[0.06+m/3.*0.97,0.04+n/3.*0.97,0.06+(m+1.)/3.*0.97-0.045,0.04+(n+1.)/3.*0.97-0.02]
  endfor
endfor
temp=pos[*,0:2]
pos[*,0:2]=pos[*,6:8]
pos[*,6:8]=temp
endif else begin
pos=make_array(4,6,/float)
for m=0,2 do begin 
  for n=0,1 do begin
    pos[*,m+3*n]=[0.06+m/3.*0.97,0.06+n/2.*0.97,0.06+(m+1.)/3.*0.97-0.045,0.06+(n+1.)/2.*0.97-0.04]
  endfor
endfor
temp=pos[*,0:2]
pos[*,0:2]=pos[*,3:5]
pos[*,3:5]=temp
endelse

cgps_open,psname,font=!p.font,/portrait,/quiet,default_thickness=1.0,charsize=0.6
if nn ne 2 then begin
  cgDisplay,xsize=2000,ysize=1900
  i0=nn*9
  i1=nn*9+8
endif else begin
  cgDisplay, xsize=2000, ysize=1270
  i0=nn*9
  i1=nn*9+3
endelse

for i=i0, i1 do begin
  
  p=pos[*,i-nn*9]

  szmax=10.0
  area=double([area_x[i],area_y[i]])
  area=double(area<szmax)
  area=area>0.5
  print,'Begin:'+strcompress(string(i+1,format='(i)'))
 
  source=strcompress(string(l[i],format='(f7.3)')+string(b[i],format='(f+6.3)'),/remove_all)
  sourcemini='G'+strcompress(string(round(l[i]*10)/10.0,format='(f5.1)')+string(round(b[i]*10)/10.0,format='(f+4.1)'),/remove_all)
  if source eq '45.467+0.053' then sourcemini='G45.5+O.O'
  
  fitspath='./fitsfiles/'
  DLHUname=fitspath+'DLH_'+sourcemini+'_U.fits'
  DLHLname=fitspath+'DLH_'+sourcemini+'_L.fits'
  DLHL2name=fitspath+'DLH_'+sourcemini+'_L2.fits

  if spec_x[i] eq -999 then fspecxsz=area[0]/480d else fspecxsz=spec_x[i]/2d
  if spec_y[i] eq -999 then fspecysz=area[1]/480d else fspecysz=spec_y[i]/2d

  cropfits,DLHUname,[l[i]-fspecxsz,l[i]+fspecxsz],[b[i]-fspecysz,b[i]+fspecysz],[v[i]-v_width[i],v[i]+v_width[i]],output='stempU'
  fits_read,'stempU_C.fits',sdatU,shdrU
  
  cropfits,DLHUname,[l[i]-fspecxsz,l[i]+fspecxsz],[b[i]-fspecysz,b[i]+fspecysz],[v1[i],v2[i]],output='ctempU'
  fits_read,'ctempU_C.fits',cdatU,chdrU
  
  cv3U=double(sxpar(shdrU,'CRVAL3'))
  cp3U=double(sxpar(shdrU,'CRPIX3'))
  dl3U=double(sxpar(shdrU,'CDELT3'))
  
  cropfits,DLHLname,[l[i]-fspecxsz,l[i]+fspecxsz],[b[i]-fspecysz,b[i]+fspecysz],[v[i]-v_width[i],v[i]+v_width[i]],output='stempL'
  fits_read,'stempL_C.fits',sdatL,shdrL
  cropfits,DLHLname,[l[i]-fspecxsz,l[i]+fspecxsz],[b[i]-fspecysz,b[i]+fspecysz],[v1[i],v2[i]],output='ctempL'
  fits_read,'ctempL_C.fits',cdatL,chdrL
  cv3L=double(sxpar(shdrL,'CRVAL3'))
  cp3L=double(sxpar(shdrL,'CRPIX3'))
  dl3L=double(sxpar(shdrL,'CDELT3'))  
  
  cropfits,DLHL2name,[l[i]-fspecxsz,l[i]+fspecxsz],[b[i]-fspecysz,b[i]+fspecysz],[v[i]-v_width[i],v[i]+v_width[i]],output='stempL2'
  fits_read,'stempL2_C.fits',sdatL2,shdrL2
  cv3L2=double(sxpar(shdrL2,'CRVAL3'))
  cp3L2=double(sxpar(shdrL2,'CRPIX3'))
  dl3L2=double(sxpar(shdrL2,'CDELT3'))

  cropfits,DLHUname,[l[i]-area[0]/120d,l[i]+area[0]/120d],[b[i]-area[1]/120d,b[i]+area[1]/120d],[v1[i],v2[i]],output='tempU'
  fits_read,'tempU_C.fits',datU,hdrU
  iimapU=total(datU[*,*,*],3)
  imageU=bytscl(iimapU,min=0.1,max=max(iimapU))
  
  cropfits,DLHLname,[l[i]-area[0]/120d,l[i]+area[0]/120d],[b[i]-area[1]/120d,b[i]+area[1]/120d],[v1[i],v2[i]],output='tempL'
  fits_read,'tempL_C.fits',datL,hdrL
  iimapL=total(datL[*,*,*],3)
  imageL=bytscl(iimapL,min=0.1,max=max(iimapL))

  wiseBname='wise_4.6_'+source+'.fits'
  wiseGname='wise_12_'+source+'.fits'
  wiseRname='wise_22_'+source+'.fits'
  wiseRGBfits=[wiseBname,wiseGname,wiseRname]
  wisesurvey=['WISE 4.6','WISE 12','WISE 22']
  foreach wisename, wiseRGBfits, index do begin
    if ~file_test('./wisedata/'+wisename) then begin
      print,' Downloading '+wisename+' from SURVEY '+wisesurvey[index]
      shellcmd='./skvbatch_wget file='+wisename+" position='"+string([l[i],b[i]],format='(f7.3,",",f6.3)')+"' Survey='"+wisesurvey[index]+$
        "' Coordinates='Galactic' Projection='Car' Pixels=600 Size="+string([szmax/60.0,szmax/60.0],format='(f7.5,",",f7.5)')
      ;print,shellcmd
      spawn,shellcmd
      fileinfo=file_info(wisename)
      while fileinfo.size lt 1000000 do begin
        spawn, 'rm ./'+wisename
        spawn,shellcmd
        fileinfo=file_info(wisename)
      endwhile
      spawn,'mv wise*.fits ./wisedata/'
    endif
  endforeach
  
  whdr=headfits('./wisedata/'+wiseBname)
  
  crp1=sxpar(hdrU,'CRPIX1')
  crv1=sxpar(hdrU,'CRVAL1')
  del1=sxpar(hdrU,'CDELT1')
  l_l=(0.5-crp1)*del1+crv1
  l_r=(sxpar(hdrU,'NAXIS1')+0.5-crp1)*del1+crv1
  cl_l=l_l+sxpar(whdr,'CDELT1')
  cl_r=l_r-sxpar(whdr,'CDELT1')
  
  crp2=sxpar(hdrU,'CRPIX2')
  crv2=sxpar(hdrU,'CRVAL2')
  del2=sxpar(hdrU,'CDELT2')
  b_d=(0.5-crp2)*del2+crv2
  b_u=(sxpar(hdrU,'NAXIS2')+0.5-crp2)*del2+crv2
  cb_d=b_d+sxpar(whdr,'CDELT2')
  cb_u=b_u-sxpar(whdr,'CDELT2')
  

  l_range=[l_l,l_r]
  b_range=[b_d,b_u]
  
 if LINE[i] eq '12CO' then begin
  if bv1[i] eq -999 || bv2[i] eq -999 then begin 
    print, 'No blue wings.' 
  endif else begin
    bluelobe=[bv1[i],bv2[i]]
    endelse
  if rv1[i] eq -999 || rv2[i] eq -999 then begin
    print, 'No red wings.' 
  endif else begin
    redlobe=[rv1[i],rv2[i]]
    endelse
 endif
  
 if LINE[i] eq '13CO' then begin
  if bv1[i] eq -999 || bv2[i] eq -999 then begin 
    print, 'No blue wings.' 
  endif else begin
    bluelobe=[bv1[i],bv2[i]]
    endelse
  if rv1[i] eq -999 || rv2[i] eq -999 then begin
    print, 'No red wings.' 
  endif else begin
    redlobe=[rv1[i],rv2[i]]
    endelse
 endif
  
  if LINE[i] eq '12CO' then begin
    if bv1[i] eq -999 || bv2[i] eq -999 then begin 
    print, 'No blue CONTOURS.' 
    endif else begin
    bl_rg=coord2pix(hdrU,bluelobe*1000.0,3)
    blueimage=datU[*,*,bl_rg[0]:bl_rg[1]]
    biicon0=total(blueimage[*,*,*],3)*abs(sxpar(hdrU,'CDELT3'))/1000.0
    biisz=size(biicon0)
    biicon=temporary(congrid(biicon0,4*biisz[1],4*biisz[2],cubic=-0.3,/minus_one))
    bmax=max(biicon0[fix(biisz[1]/3):fix(2*biisz[1]/3),fix(biisz[2]/3):fix(2*biisz[2]/3)])
    levelsign=where(sign3 eq sign1[i])
    levelsign=long(mean(levelsign))
    if levelsign ne -1 then begin
      dbcon=bmax*dblev[levelsign]/10.0
      bcon0=bmax*blev0[levelsign]
    endif else begin
      dbcon=bmax*1.0/10.0
      bcon0=bmax*0.4
    endelse
    levels1=indgen(10)*dbcon+bcon0
    endelse
    
    ;redlobe
    if rv1[i] eq -999 || rv2[i] eq -999 then begin 
    print, 'No red CONTOURS.' 
    endif else begin
    rl_rg=coord2pix(hdrU,redlobe*1000.0,3)
    riicon0=total(datU[*,*,rl_rg[0]:rl_rg[1]],3)*abs(sxpar(hdrU,'CDELT3'))/1000.0
    riisz=size(riicon0)
    riicon=temporary(congrid(riicon0,4*riisz[1],4*riisz[2],cubic=-0.3,/MINUS_ONE))
    rmax=max(riicon0[fix(riisz[1]/3):fix(2*riisz[1]/3),fix(riisz[2]/3):fix(2*riisz[2]/3)])
    levelsign=where(sign3 eq sign1[i])
    levelsign=long(mean(levelsign))
    if levelsign ne -1 then begin
      drcon=rmax*drlev[levelsign]/10.0
      rcon0=rmax*rlev0[levelsign]
    endif else begin
      drcon=rmax*1.0/10.0
      rcon0=rmax*0.4
    endelse
    levels2=indgen(10)*drcon+rcon0
    endelse
    
  endif
  
  if LINE[i] eq '13CO' then begin
    ;bluelobe
    if bv1[i] eq -999 || bv2[i] eq -999 then begin 
    print, 'No blue CONTOURS.' 
    endif else begin
    bl_rg=coord2pix(hdrL,bluelobe*1000.0,3)
    blueimage=datL[*,*,bl_rg[0]:bl_rg[1]]
    biicon0=total(blueimage[*,*,*],3)*abs(sxpar(hdrL,'CDELT3'))/1000.0
    biisz=size(biicon0)
    biicon=temporary(congrid(biicon0,4*biisz[1],4*biisz[2],cubic=-0.3,/minus_one))
    bmax=max(biicon0[fix(biisz[1]/3):fix(2*biisz[1]/3),fix(biisz[2]/3):fix(2*biisz[2]/3)])
    levelsign=where(sign3 eq sign1[i])
    levelsign=long(mean(levelsign))
    if levelsign ne -1 then begin
      dbcon=bmax*dblev[levelsign]/10.0
      bcon0=bmax*blev0[levelsign]
    endif else begin
      dbcon=bmax*1.0/10.0
      bcon0=bmax*0.4
    endelse
    levels1=indgen(10)*dbcon+bcon0
    endelse
    
    ;redlobe
    if rv1[i] eq -999 || rv2[i] eq -999 then begin 
    print, 'No red CONTOURS.' 
    endif else begin
    rl_rg=coord2pix(hdrL,redlobe*1000.0,3)
    riicon0=total(datL[*,*,rl_rg[0]:rl_rg[1]],3)*abs(sxpar(hdrL,'CDELT3'))/1000.0
    riisz=size(riicon0)
    riicon=temporary(congrid(riicon0,4*riisz[1],4*riisz[2],cubic=-0.3,/MINUS_ONE))
    rmax=max(riicon0[fix(riisz[1]/3):fix(2*riisz[1]/3),fix(riisz[2]/3):fix(2*riisz[2]/3)])
    levelsign=where(sign3 eq sign1[i])
    levelsign=long(mean(levelsign))
    if levelsign ne -1 then begin
      drcon=rmax*drlev[levelsign]/10.0
      rcon0=rmax*rlev0[levelsign]
    endif else begin
      drcon=rmax*1.0/10.0
      rcon0=rmax*0.4
    endelse
    levels2=indgen(10)*drcon+rcon0
    endelse  

  endif
 

  cropfits,'./wisedata/'+wiseBname,[cl_r,cl_l],[cb_u,cb_d],output='tempwiseB'
  cropfits,'./wisedata/'+wiseGname,[cl_r,cl_l],[cb_u,cb_d],output='tempwiseG'
  cropfits,'./wisedata/'+wiseRname,[cl_r,cl_l],[cb_u,cb_d],output='tempwiseR'

  fits_read,'tempwiseB_C.fits',wiseB,whdrB
  ;wiseimg_B=reform(bytscl(wiseB,min=max(wiseB)*0.1,max=max(wiseB)),1,n_elements(wiseB[*,0]),n_elements(wiseB[0,*]))
  ;wiseimg_B=reform(logscl(wiseB,min=min(wiseB)*0.6,max=min(wiseB)+(max(wiseB)-min(wiseB))*0.9),1,n_elements(wiseB[*,0]),n_elements(wiseB[0,*]))
  wiseimg_B=wisescl(wiseB)
  
  fits_read,'tempwiseG_C.fits',wiseG
  ;wiseimg_G=reform(bytscl(wiseG,min=max(wiseG)*0.1,max=max(wiseG)),1,n_elements(wiseG[*,0]),n_elements(wiseG[0,*]))
  ;wiseimg_G=reform(logscl(wiseG,min=min(wiseG)*0.6,max=min(wiseG)+(max(wiseG)-min(wiseG))*0.9),1,n_elements(wiseG[*,0]),n_elements(wiseG[0,*]))
  wiseimg_G=wisescl(wiseG)
  
  fits_read,'tempwiseR_C.fits',wiseR
  ;wiseimg_R=reform(bytscl(wiseR,min=max(wiseR)*0.1,max=max(wiseR)),1,n_elements(wiseR[*,0]),n_elements(wiseR[0,*]))
  ;wiseimg_R=reform(logscl(wiseR,min=min(wiseR)*0.6,max=min(wiseR)+(max(wiseR)-min(wiseR))*0.9),1,n_elements(wiseR[*,0]),n_elements(wiseR[0,*]))
  wiseimg_R=wisescl(wiseR)
  
  wiseimg_RGB=[wiseimg_R,wiseimg_G,wiseimg_B]
  
  wcrp1=sxpar(whdrB,'CRPIX1')
  wcrv1=sxpar(whdrB,'CRVAL1')
  wdel1=sxpar(whdrB,'CDELT1')
  wl_l=(0.5-wcrp1)*wdel1+wcrv1
  wl_r=(sxpar(whdrB,'NAXIS1')+0.5-wcrp1)*wdel1+wcrv1
  wcrp2=sxpar(whdrB,'CRPIX2')
  wcrv2=sxpar(whdrB,'CRVAL2')
  wdel2=sxpar(whdrB,'CDELT2')
  wb_d=(0.5-wcrp2)*wdel2+wcrv2
  wb_u=(sxpar(whdrB,'NAXIS2')+0.5-wcrp2)*wdel2+wcrv2


  if i eq 6 then cgplot,[0],[0],/nodata,aspect=area[1]/area[0],xrange=l_range,yrange=b_range,position=p,$;pos[*,i],$
    xtickinterval=round((max(l_range)-min(l_range))*10*area[1]/area[0])/50d,ytickinterval=round((max(b_range)-min(b_range))*10)/50d,$
    xtitle=textoidl('Galactic Longitude (^{o})'),ytitle='Galactic Latitude (!Uo!N)',/noerase else $
    cgplot,[0],[0],/nodata,/noerase,aspect=area[1]/area[0],xrange=l_range,yrange=b_range,$
    xtickinterval=round((max(l_range)-min(l_range))*10*area[1]/area[0])/50d,ytickinterval=round((max(b_range)-min(b_range))*10)/50d,$
    position=p;pos[*,i]
  
  cgimage,wiseimg_RGB,/noerase,position=p;pos[*,i]
  
  cgplot,[0],[0], xrange=l_range,yrange=b_range,xtickinterval=round((max(l_range)-min(l_range))*10*area[1]/area[0])/50d,ytickinterval=round((max(b_range)-min(b_range))*10)/50d,$;, xtitle='Galactic Longitude (!Uo!N)',$
  /nodata,AxisColor=cgcolor('brown'),/noerase,aspect=area[1]/area[0],xtickformat='(a1)',ytickformat='(a1)',position=p;pos[*,i]
  
  if LINE[i] eq '12CO' then begin
  for n=0, n_elements(iimapU[*,0])-1 do begin
    xgrid=pix2coord(hdrU,make_array(n_elements(iimapU[*,0]),value=n,/int),1)
    ygrid=pix2coord(hdrU,make_array(n_elements(iimapU[0,*]),/int,/index),2)
    ;cgplot,xgrid,ygrid,/overplot,psym=1,color='brown',symsize=0.5
  endfor
  endif
  
  if LINE[i] eq '13CO' then begin
  for n=0, n_elements(iimapL[*,0])-1 do begin
    xgrid=pix2coord(hdrL,make_array(n_elements(iimapL[*,0]),value=n,/int),1)
    ygrid=pix2coord(hdrL,make_array(n_elements(iimapL[0,*]),/int,/index),2)
    ;cgplot,xgrid,ygrid,/overplot,psym=1,color='brown',symsize=0.5
  endfor
  endif
  
  if LINE[i] eq '12CO' then begin
    if bv1[i] eq -999 || bv2[i] eq -999 then begin 
      print, 'No blue CONTOURS.' 
    endif else begin
      cgloadct,33,clip=[0,60],ncolors=10,/reverse
      cgContour,biicon,label=0,/onimage,level=levels1,c_colors=indgen(10)
    endelse
    if rv1[i] eq -999 || rv2[i] eq -999 then begin 
    print, 'No red CONTOURS.' 
    endif else begin
      cgloadct,33,clip=[195,255],ncolors=10
      cgContour,riicon,label=0,/onimage,level=levels2,c_linestyle=2,c_colors=indgen(10)
    endelse
  endif
  
  if LINE[i] eq '13CO' then begin
    if bv1[i] eq -999 || bv2[i] eq -999 then begin 
    print, 'No blue CONTOURS.' 
    endif else begin
      cgloadct,33,clip=[0,60],ncolors=10,/reverse
      cgContour,biicon,c_colors=indgen(10),label=0,/onimage,level=levels1
    endelse
    if rv1[i] eq -999 || rv2[i] eq -999 then begin 
    print, 'No red CONTOURS.' 
    endif else begin
      cgloadct,33,clip=[195,255],ncolors=10
      cgContour,riicon,c_colors=indgen(10),label=0,/onimage,c_linestyle=2,level=levels2
    endelse
  endif
  
  sz_scale=1/60.0
  x0_pos=!x.CRange[1]+0.05*(!x.CRange[0]-!x.CRange[1])
  x1_pos=!x.CRange[1]+0.05*(!x.CRange[0]-!x.CRange[1])+sz_scale
  y0_pos=!y.CRange[1]-0.05*(!y.CRange[1]-!y.CRange[0])
  y1_pos=y0_pos
  x_txt=(x0_pos+x1_pos)/2
  y_txt=!y.CRange[1]-0.1*(!y.CRange[1]-!y.CRange[0])
  x_txt1=!x.CRange[0]-0.05*(!x.CRange[0]-!x.CRange[1])
  y_txt1=!y.CRange[0]+0.05*(!y.CRange[1]-!y.CRange[0])
  x_txt2=x_txt1
  y_txt2=!y.CRange[0]+0.1*(!y.CRange[1]-!y.CRange[0])
  if LINE[i] eq '12CO' then cgtext,x_txt2,y_txt2,'!U12!NCO(1-0) contours',color='black',charthick=2
  if LINE[i] eq '13CO' then cgtext,x_txt2,y_txt2,'!U13!NCO(1-0) contours',color='black',charthick=2
  if LINE[i] eq '12CO' then cgtext,x_txt2,y_txt2,'!U12!NCO(1-0) contours',color='yellow'
  if LINE[i] eq '13CO' then cgtext,x_txt2,y_txt2,'!U13!NCO(1-0) contours',color='yellow'
  x_txt3=x_txt1
  y_txt3=!y.CRange[1]-0.075*(!y.CRange[1]-!y.CRange[0])
  cgtext,x_txt3,y_txt3,'G'+source,color='black',charthick=2
  cgplot,[x0_pos,x1_pos],[y0_pos,y1_pos],/overplot,LineStyle=0, Color='black',thick=4
  cgtext,x_txt,y_txt,string(sz_scale*(!pi/180)*1000.0*d[i],format='(f4.2)')+' pc',alignment=0.5,color='black',charthick=2
  cgtext,x_txt1,y_txt1,'WISE 4.6 12 22 !4l!Xm',color='black',charthick=2
  cgtext,x_txt3,y_txt3,'G'+source,color='black',charthick=2
  
  cgtext,x_txt3,y_txt3,'G'+source,color='yellow'
  cgplot,[x0_pos,x1_pos],[y0_pos,y1_pos],/overplot,LineStyle=0, Color='yellow',thick=3
  cgtext,x_txt,y_txt,string(sz_scale*(!pi/180)*1000.0*d[i],format='(f4.2)')+' pc',alignment=0.5,color='yellow'
  cgtext,x_txt1,y_txt1,'WISE 4.6 12 22 !4l!Xm',color='yellow'
  cgtext,x_txt3,y_txt3,'G'+source,color='yellow'
  
  cgplot,l[i],b[i],psym=cgSymCat(45,thick=1.0),/overplot,symcolor=cgcolor('white'),symsize=1.5,/data
  cgplot,l[i],b[i],psym=cgSymCat(46),/overplot,symcolor=cgcolor('brown'),symsize=1.5,/data
  
  j=where(sign2 eq sign1[i])
  if j ne -1 then begin
  if bv1[i] ne -999 && bv2[i] ne -999 then begin
    cgplot,/overplot,[blue_l[j]-bszx[j]/2.0,blue_l[j]+bszx[j]/2.0,blue_l[j]+bszx[j]/2.0,blue_l[j]-bszx[j]/2.0,blue_l[j]-bszx[j]/2.0],$
      [blue_b[j]-bszy[j]/2.0,blue_b[j]-bszy[j]/2.0,blue_b[j]+bszy[j]/2.0,blue_b[j]+bszy[j]/2.0,blue_b[j]-bszy[j]/2.0],$
      color='white',thick=3
    cgplot,/overplot,[blue_l[j]-bszx[j]/2.0,blue_l[j]+bszx[j]/2.0,blue_l[j]+bszx[j]/2.0,blue_l[j]-bszx[j]/2.0,blue_l[j]-bszx[j]/2.0],$
      [blue_b[j]-bszy[j]/2.0,blue_b[j]-bszy[j]/2.0,blue_b[j]+bszy[j]/2.0,blue_b[j]+bszy[j]/2.0,blue_b[j]-bszy[j]/2.0],$
      color='blue'
  endif
  if rv1[i] ne -999 && rv2[i] ne -999 then begin
    cgplot,/overplot,[red_l[j]-rszx[j]/2.0,red_l[j]+rszx[j]/2.0,red_l[j]+rszx[j]/2.0,red_l[j]-rszx[j]/2.0,red_l[j]-rszx[j]/2.0],$
      [red_b[j]-rszy[j]/2.0,red_b[j]-rszy[j]/2.0,red_b[j]+rszy[j]/2.0,red_b[j]+rszy[j]/2.0,red_b[j]-rszy[j]/2.0],$
      color='white',thick=3
    cgplot,/overplot,[red_l[j]-rszx[j]/2.0,red_l[j]+rszx[j]/2.0,red_l[j]+rszx[j]/2.0,red_l[j]-rszx[j]/2.0,red_l[j]-rszx[j]/2.0],$
      [red_b[j]-rszy[j]/2.0,red_b[j]-rszy[j]/2.0,red_b[j]+rszy[j]/2.0,red_b[j]+rszy[j]/2.0,red_b[j]-rszy[j]/2.0],$
      color='red'
  endif
  endif
 
  spawn,'rm *temp*_C.fits'
  if file_test('*mosaic*.fits') then spawn,'rm *mosaic*.fits'
  if file_test('*_rms.fits') then spawn,'rm *_rms.fits'
  if file_test('*lobe_C.fits') then spawn,'rm *lobe_C.fits'
  print,'Done!'
endfor

cgps_close

if ~file_test('./new_figure') then spawn,'mkdir new_figure'
if file_test(psname) then spawn,'mv '+psname+' ./new_figure'

endfor

end