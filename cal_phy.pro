pro cal_phy

;read outflowcat.cat
;read blue/red_out.cat
;read corresponding fits
;draw figures
;calculations

cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/'

;if ~keyword_set(region) then region='BFS52'
;if ~keyword_set(Lvrange) then Lvrange=[5,11]
;distance=2
;if ~keyword_set(region) then region='GGMC1'
;if ~keyword_set(Lvrange) then Lvrange=[2,9]
;distance=2
;if ~keyword_set(region) then region='GGMC2'
;if ~keyword_set(Lvrange) then Lvrange=[5,11]
;distance=2
;if ~keyword_set(region) then region='GGMC3'
;if ~keyword_set(Lvrange) then Lvrange=[5,11]
;distance=2
;if ~keyword_set(region) then region='GGMC4'
;if ~keyword_set(Lvrange) then Lvrange=[-3,5]
;distance=2
;if ~keyword_set(region) then region='lynds'
;if ~keyword_set(Lvrange) then Lvrange=[-3,3]
;distance=0.4
if ~keyword_set(region) then region='west'
if ~keyword_set(Lvrange) then Lvrange=[-1,4]
distance=0.6
;if ~keyword_set(region) then region='swallow'
;if ~keyword_set(Lvrange) then Lvrange=[12,18]
;distance=3.8
;if ~keyword_set(region) then region='horn'
;if ~keyword_set(Lvrange) then Lvrange=[12,18]
;distance=3.8
;if ~keyword_set(region) then region='remote'
;if ~keyword_set(Lvrange) then Lvrange=[18,28]
;distance=8.5

cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/'+region+'/candidates'
L_lim=0.6
def_rmsU=0.25
def_rmsL=0.3
outsz=(8./distance)>3
outsz=outsz<5

readcol,'blue_out.cat',bsn,bpeak1,bpeak2,bw0,bw1,bnote,format='I,F,F,F,F,A',stringskip='#'
readcol,'red_out.cat',rsn,rpeak1,rpeak2,rw0,rw1,rnote,format='I,F,F,F,F,A',stringskip='#'
readcol,'outflowcat.cat',num,glon,glat,bluev0,bluev1,redv0,redv1,tag,bnum,rnum,format='I,F,F,F,F,F,F,A,I,I',stringskip='#'

;openw,lobeb,/get_lun,'lobe_blue.cat'
;openw,lober,/get_lun,'lobe_red.cat'
;printf,lobeb,'# bn','Glon','Glat','bw[0]','bw[1]','spec','pv','con',$
;  format='(a4,a9,a7,2a6,3a5)'
;printf,lober,'# rn','Glon','Glat','rw[0]','rw[1]','spec','pv','con',$
;  format='(a4,a9,a7,2a6,3a5)'
;for i=0, n_elements(bsn)-1 do printf,lobeb,bsn[i],bpeak1[i],bpeak2[i],bw0[i],bw1[i],'-','-','-',$
;  format='(i4,f9.3,f7.3,2f6.1,3a5)'
;for i=0, n_elements(rsn)-1 do printf,lober,rsn[i],rpeak1[i],rpeak2[i],rw0[i],rw1[i],'-','-','-',$
;  format='(i4,f9.3,f7.3,2f6.1,3a5)'
;free_lun,lobeb,lober
;
;openw,para,/get_lun,'lobe_info.cat'
;printf,para,'#osn','tag','lsn','Glon','Glat','w0','w1','spec','pv','con','rank',$
;  format='(3a4,a9,a7,2a8,4a6)'
;;printf,para,'#####################################################################'
;for i=0, n_elements(num)-1 do begin
;  j=where(bsn eq bnum[i])
;  if j ne -1 then printf,para,num[i],'B',bsn[j],bpeak1[j],bpeak2[j],bw0[j],bw1[j],'-','-','-','-',$
;    format='(i4,a4,i4,f9.3,f7.3,2f8.1,4a6)' $
;  else printf,para,num[i],'B','-','-','-','-','-','-','-','-','-',format='(i4,2a4,a9,a7,2a8,4a6)'
;  k=where(rsn eq rnum[i])
;  if k ne -1 then printf,para,'    ','R',rsn[k],rpeak1[k],rpeak2[k],rw0[k],rw1[k],'-','-','-','-',$
;    format='(2a4,i4,f9.3,f7.3,2f8.1,4a6)' $
;  else printf,para,'    ','R','-','-','-','-','-','-','-','-','-',format='(3a4,a9,a7,2a8,4a6)'
;  ;printf,para,'#####################################################################'
;endfor
;free_lun,para

fits_read,'../'+region+'_Ua_C.fits',datUa,hdrUa
fits_read,'../'+region+'_La_C.fits',datLa,hdrLa
;fits_read,region+'_L2a_mask.fits',datL2a,hdrL2a
fits_read,'../Lpeakv0.fits',peakvmap,vhdr
fits_read,'../Ubvmap.fits',Ubvmap,bvhdr
fits_read,'../Urvmap.fits',Urvmap,rvhdr
dathdr=list(datUa,hdrUa)
dathdrL=list(datLa,hdrLa)

pixU=double(sxpar(hdrUa,'CRPIX3'))
delU=double(sxpar(hdrUa,'CDELT3'))
crvU=double(sxpar(hdrUa,'CRVAL3'))
pixL=double(sxpar(hdrLa,'CRPIX3'))
delL=double(sxpar(hdrLa,'CDELT3'))
crvL=double(sxpar(hdrLa,'CRVAL3'))
specU=reform(datUa[0,0,*])
specL=reform(datLa[0,0,*])
vU=((indgen(n_elements(specU))+1-pixU)*delU+crvU)/1000.0
vL=((indgen(n_elements(specL))+1-pixL)*delL+crvL)/1000.0
  
openw,phy,/get_lun,'derived_phy.cat'
printf,phy,'#osn','tag','lsn','Glon','Glat','area','acm2','low','len','v_c','del_v','mass','moment','E','t','L_flow',$
  format='(3a4,a9,a7,2a7,2a7,2a7,5a11)'
openw,con,/get_lun,'con_start.cat'
;printf,con,'#num','tag','bsn','rsn','blow','rlow',format='(4a4,2a7)'
printf,con,'#  blow','bmax','rlow','rmax',format='(4a7)'


;measure physical properties
T_ex=30

for i=0, n_elements(num)-1 do begin
  j=where(bsn eq bnum[i])
  k=where(rsn eq rnum[i])
  if j ne -1 then begin
    cropfits,/dataform,dathdr,[bpeak1[j]+outsz/60.,bpeak1[j]-outsz/60.],[bpeak2[j]-outsz/60.,bpeak2[j]+outsz/60.],[bw0[j],bw1[j]],output=bcr
    bcv3=double(sxpar(bcr.hdr,'CRVAL3'))
    bcp3=double(sxpar(bcr.hdr,'CRPIX3'))
    bdl3=double(sxpar(bcr.hdr,'CDELT3'))
    bdata=smooth(bcr.dat,[1,1,3],/edge_mirror)
    biimap=total(bdata,3)*abs(bdl3)/1000.
    biimap=congrid(biimap,3*n_elements(biimap[*,0]),3*n_elements(biimap[0,*]),cubic=-0.5,/MINUS_ONE)
    biimap=smooth(biimap,[3,3],/edge_mirror)
    bmax=max(biimap[round(n_elements(biimap[*,0])/2.-n_elements(biimap[*,0])/12.-1):round(n_elements(biimap[*,0])/2.+n_elements(biimap[*,0])/12.-1),$
    round(n_elements(biimap[0,*])/2.-n_elements(biimap[0,*])/12.-1):round(n_elements(biimap[0,*])/2.+n_elements(biimap[0,*])/12.-1)])
    ;area,length
    db_area=((0.5/180.0)*(!pi/180)*1000.0*distance)^2
    cal_area,biimap,lmt=0.45,area=barea,mask=bmask,low=blow
    blue_area=barea*db_area
    blue_size=barea*(0.5/3.)^2
    blen=1.25*sqrt(blue_area)
    ;mean integrated intensity
    b_I=total(biimap*bmask)/total(bmask)
    ;delta velocity
    vLpeak=mean(peakvmap[(coord2pix(hdrLa,bpeak1[j],1)-1):(coord2pix(hdrLa,bpeak1[j],1)+1),$
      (coord2pix(hdrLa,bpeak2[j],2)-1):(coord2pix(hdrLa,bpeak2[j],2)+1)],/nan)
    if finite(vLpeak,/nan) eq 1 then begin
      vLpeak=Lvrange[0]
      print,'ALEEEEEEEEEEEEEERT!!!!!!!!!!!!!!!!!!!!!!!!!!'
    endif
    blue_Dv=bw0[j]-vLpeak
    blue_t=abs(blen/blue_Dv)*3.086e13/(365*24*3600.0)
    ;bcv3=double(sxpar(bcr.hdr,'CRVAL3'))
    ;bcp3=double(sxpar(bcr.hdr,'CRPIX3'))
    ;bdl3=double(sxpar(bcr.hdr,'CDELT3'))
    ;bspec_mean=total(bdata[round((size(bdata))[1]/2.-1):round((size(bdata))[1]/2.+1),round((size(bdata))[2]/2.-1):round((size(bdata))[2]/2.+1)],1)
    ;bspec_mean=total(bspec_mean,1)/4.
    ;blue_v=((indgen(n_elements(bspec_mean))+1-bcp3)*bdl3+bcv3)/1000.0
    ;mean_bv=total(bspec_mean*(blue_v-vLpeak))/total(bspec_mean)
    ;mean_bv2=total(bspec_mean*((blue_v-vLpeak)^2))/total(bspec_mean)
    mean_bv=bw0[j]-vLpeak+(bw1[j]-bw0[j])/2.5
    mean_bv2=mean_bv^2
    
    ;physical parameters
    bNH2=4.2e17*b_I*T_ex/exp(-5.5/T_ex)
    bMH2=1.0/6.3e19*bNH2*blue_area
    bPH2=bMH2*abs(mean_bv)
    bEH2=0.5*bMH2*(mean_bv2)*1.9818E30*1.0E6*1e7 ;[J=kg*m^2*s^-2]
    bLm=bEH2/(blue_t*365*24*3600)
    
    printf,phy,num[i],'B',bsn[j],bpeak1[j],bpeak2[j],blue_area,blue_size,blow,blen,vLpeak,mean_bv,bMH2,bPH2,bEH2,blue_t,bLm,$
      format='(i4,a4,i4,f9.3,f7.3,2f7.2,2f7.2,2f7.2,5e11.2)'
  endif else printf,phy,num[i],'B','-','-','-','-','-','-','-','-','-','-','-','-','-','-',format='(i4,2a4,a9,a7,2a7,4a7,5a11)'
  
  
  if k ne -1 then begin
    cropfits,/dataform,dathdr,[rpeak1[k]+outsz/60.,rpeak1[k]-outsz/60.],[rpeak2[k]-outsz/60.,rpeak2[k]+outsz/60.],[rw0[k],rw1[k]],output=rcr
    rdata=smooth(rcr.dat,[1,1,3],/edge_mirror)
    riimap=total(rdata,3)*abs(sxpar(rcr.hdr,'CDELT3'))/1000.
    riimap=congrid(riimap,3*n_elements(riimap[*,0]),3*n_elements(riimap[0,*]),cubic=-0.5,/MINUS_ONE)
    riimap=smooth(riimap,[3,3],/edge_mirror)
    rmax=max(riimap[round(n_elements(riimap[*,0])/2.-n_elements(riimap[*,0])/12.-1):round(n_elements(riimap[*,0])/2.+n_elements(riimap[*,0])/12.-1),$
    round(n_elements(riimap[0,*])/2.-n_elements(riimap[0,*])/12.-1):round(n_elements(riimap[0,*])/2.+n_elements(riimap[0,*])/12.-1)])
    dr_area=((0.5/180.0)*(!pi/180)*1000.0*distance)^2
    cal_area,riimap,lmt=0.45,area=rarea,mask=rmask,low=rlow
    red_area=rarea*dr_area
    red_size=rarea*(0.5/3.)^2
    rlen=1.25*sqrt(red_area)
    ;print,red_area
    R_I=total(Riimap*Rmask)/total(Rmask)
    ;delta velocity
    vLpeak=mean(peakvmap[(coord2pix(hdrLa,rpeak1[k],1)-1):(coord2pix(hdrLa,rpeak1[k],1)+1),$
      (coord2pix(hdrLa,rpeak2[k],2)-1):(coord2pix(hdrLa,rpeak2[k],2)+1)],/nan)
    if finite(vLpeak,/nan) eq 1 then begin
      vLpeak=Lvrange[1]
      print,'ALEEEEEEEEEEEEEERT!!!!!!!!!!!!!!!!!!!!!!!!!!'
    endif
    red_Dv=rw1[k]-vLpeak
    red_t=abs(rlen/red_Dv)*3.086e13/(365*24*3600.0)
    ;mean lobe velocity
    ;rcv3=double(sxpar(rcr.hdr,'CRVAL3'))
    ;rcp3=double(sxpar(rcr.hdr,'CRPIX3'))
    ;rdl3=double(sxpar(rcr.hdr,'CDELT3'))
    ;rspec_mean=total(rdata[round((size(rdata))[1]/2.-1):round((size(rdata))[1]/2.+1),round((size(rdata))[2]/2.-1):round((size(rdata))[2]/2.+1)],1)
    ;rspec_mean=total(rspec_mean,1)/4.
    ;red_v=((indgen(n_elements(rspec_mean))+1-rcp3)*rdl3+rcv3)/1000.0
    ;mean_rv=total(rspec_mean*(red_v-vLpeak))/total(rspec_mean)
    ;mean_rv2=total(rspec_mean*((red_v-vLpeak)^2))/total(rspec_mean)
    mean_rv=rw0[k]-vLpeak+(rw1[k]-rw0[k])/2.5
    mean_rv2=mean_rv^2
    
    ;physical parameters
    rNH2=4.2e17*r_I*T_ex/exp(-5.5/T_ex)
    rMH2=1.0/6.3e19*rNH2*red_area
    rPH2=rMH2*abs(mean_rv)
    rEH2=0.5*rMH2*(mean_rv2)*1.9818E30*1.0E6*1e7 ;[J=kg*m^2*s^-2]
    rLm=rEH2/(red_t*365*24*3600)
    
    printf,phy,'','R',rsn[k],rpeak1[k],rpeak2[k],red_area,red_size,rlow,rlen,vLpeak,mean_rv,rMH2,rPH2,rEH2,red_t,rLm,$
      format='(a4,a4,i4,f9.3,f7.3,2f7.2,2f7.2,2f7.2,5e11.2)'
    endif else printf,phy,'','R','-','-','-','-','-','-','-','-','-','-','-','-','-','-',format='(a4,2a4,a9,a7,2a7,4a7,5a11)'
    
    if j eq -1 then begin
      blow=0 & bmax=0
    endif
    if k eq -1 then begin
      rlow=0 & rmax=0
    endif
    ;printf,con,num[i],tag[i],bnum[i],rnum[i],blow,rlow,format='(i4,a4,2i4,2f7.2)'
    printf,con,blow,bmax,rlow,rmax,format='(4f7.2)'
endfor

free_lun,phy,con

;draw outflows


end