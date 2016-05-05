function delnoise,img
sz=size(img)
img[0,*]=0
img[-1,*]=0
img[*,0]=0
img[*,-1]=0
for i=1, sz[1]-2 do begin
  for j=1, sz[2]-2 do begin
    if img[i,j] eq 1 then begin
      if img[i+1,j] eq 0 && $
         img[i-1,j] eq 0 && $
         img[i,j+1] eq 0 && $
         img[i,j-1] eq 0 then begin
         img[i,j]=0
       endif
    endif
  endfor
endfor
return,img
end

pro draw_spectra
namelist=['GGMC1','GGMC2','GGMC3','GGMC4','BFS52','Lynds','West Front','Swallow','Horn','Remote Clouds']
names=['GGMC1','GGMC2','GGMC3','GGMC4','BFS52','lynds','west','swallow','horn','remote']
dists=[2,2,2,2,2,0.4,0.6,3.8,3.8,8.5]
;xranges=list($
;  [-4,11],
;  [-6,7],
cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/';0data/'
readcol,'0data/region',name,Uv0,Uv1,Lv0,Lv1,L2v0,L2v1,format='A,F,F,F,F,F,F',stringskip='#'

psname='spectra.ps'
cgps_open,psname,font=!p.font,/quiet,default_thickness=1.0,charsize=0.8
xsize1=1600. & ysize1=1000.
cgDisplay, xsize=round(xsize1), ysize=round(ysize1)

pos0=[0.035,      2./3+0.05,  1./4-0.015, 1.-0.02]
pos1=[1./4+0.035, 2./3+0.05,  2./4-0.015, 1.-0.02]
pos2=[2./4+0.035, 2./3+0.05,  3./4-0.015, 1.-0.02]
pos3=[3./4+0.035, 2./3+0.05,  1-0.015,    1.-0.02]

pos4=[0.035,      1./3+0.05,  1./4-0.015, 2./3-0.02]
pos5=[1./4+0.035, 1./3+0.05,  2./4-0.015, 2./3-0.02]
pos6=[2./4+0.035, 1./3+0.05,  3./4-0.015, 2./3-0.02]
pos7=[3./4+0.035, 1./3+0.05,  1-0.015,    2./3-0.02]

pos8=[0.035,      0.05,       1./4-0.015, 1./3-0.02]
pos9=[1./4+0.035, 0.05,       2./4-0.015, 1./3-0.02]


poses=list(pos0,pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,pos9)

;openw,out,/get_lun,'region_para.txt'
;printf,out,'region','sigma_1','mass','Tex',format='(a8,3a10)'

for i=0,9 do begin

region=name[i]
Uvrange=[Uv0[i],Uv1[i]]
Lvrange=[Lv0[i],Lv1[i]]
L2vrange=[L2v0[i],L2v1[i]]
;cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/'+region
;distance=3.8
fits_read,region+'/'+region+'_Ua_mask.fits',datUa,hdrUa
fits_read,region+'/'+region+'_La_mask.fits',datLa,hdrLa
fits_read,region+'/'+region+'_L2a_mask.fits',datL2a,hdrL2a

datUa=smooth(datUa,[1,1,5],/EDGE_MIRROR)
datLa=smooth(datLa,[1,1,5],/EDGE_MIRROR)
datL2a=smooth(datL2a,[1,1,3],/EDGE_MIRROR)
datUa=smooth(datUa,[1,1,5],/EDGE_MIRROR)
datLa=smooth(datLa,[1,1,5],/EDGE_MIRROR)
datL2a=smooth(datL2a,[1,1,3],/EDGE_MIRROR)

dathdrU=list(datUa,hdrUa)
dathdrL=list(datLa,hdrLa)
dathdrL2=list(datL2a,hdrL2a)

cpxU=double(sxpar(hdrUa,'CRPIX3'))
delU=double(sxpar(hdrUa,'CDELT3'))
crvU=double(sxpar(hdrUa,'CRVAL3'))
cpxL=double(sxpar(hdrLa,'CRPIX3'))
delL=double(sxpar(hdrLa,'CDELT3'))
crvL=double(sxpar(hdrLa,'CRVAL3'))
cpxL2=double(sxpar(hdrL2a,'CRPIX3'))
delL2=double(sxpar(hdrL2a,'CDELT3'))
crvL2=double(sxpar(hdrL2a,'CRVAL3'))

szU=size(datUa)
szL=size(datLa)
szL2=size(datL2a)
sz=szU

covU=make_array(sz[1],sz[2],value=0)
covL=make_array(sz[1],sz[2],value=0)
covL2=make_array(sz[1],sz[2],value=0,/BYTE)

cropfits,dathdrU,L2vrange,dim='v',/dataform,output=crU
cropfits,dathdrL,L2vrange,dim='v',/dataform,output=crL
cropfits,dathdrL2,L2vrange,dim='v',/dataform,output=crL2

for v=0, (size(crU.dat))[3]-1 do covU[where((crU.dat)[*,*,v] gt 1.5)]+=1
for v=0, (size(crL.dat))[3]-1 do covL[where((crL.dat)[*,*,v] gt 1.0)]+=1
for v=0, (size(crL2.dat))[3]-1 do covL2[where((crL2.dat)[*,*,v] gt 0.6)]+=1

covU[where(covU ne 0)]=1
covL[where(covL ne 0)]=1
covL2[where(covL2 ne 0)]=1

covU=delnoise(covU)
covL=delnoise(covL)
covL2=delnoise(covL2)

fits_write,'covU.fits',covU,hdrUa
fits_write,'covL.fits',covL,hdrLa
fits_write,'covL2.fits',covL2,hdrL2a

for v=0, szU[3]-1 do datUa[*,*,v]=datUa[*,*,v]*covL
for v=0, szL[3]-1 do datLa[*,*,v]=datLa[*,*,v]*covL
for v=0, szL2[3]-1 do datL2a[*,*,v]=datL2a[*,*,v]*covL2

specUa=total(datUa,1)
specUa=total(specUa,1)
specUa=specUa/total(covL)
vU=((indgen(n_elements(specUa))+1-cpxU)*delU+crvU)/1000.0

specLa=total(datLa,1)
specLa=total(specLa,1)
specLa=specLa/total(covL)
vL=((indgen(n_elements(specLa))+1-cpxL)*delL+crvL)/1000.0

specL2a=total(datL2a,1)
specL2a=total(specL2a,1)
specL2a=specL2a/total(covL2)
vL2=((indgen(n_elements(specL2a))+1-cpxL2)*delL2+crvL2)/1000.0

specL2a=gauss_smooth(specL2a,1,/edge_mirror)

;p=poses[i]
if i eq 8 then begin
  cgplot,vU,specUa,color='blue',xrange=[Uv0[i]-1.5,Uv1[i]+1.5],yrange=[0-max(specUa)*0.15,max(specUa)*1.15],psym=10,position=poses[i],/noerase,$
    title=namelist[i],xtitle=textoidl('LSR velocity (km s^{-1})'),ytitle=textoidl('T_{mb} (K)')
endif else cgplot,vU,specUa,color='blue',xrange=[Uv0[i]-1.5,Uv1[i]+1.5],yrange=[0-max(specUa)*0.15,max(specUa)*1.15],psym=10,position=poses[i],/noerase,$
  title=namelist[i]
cgplot,vL,specLa,color='green',/over,psym=10
cgplot,vL2,specL2a,color='red',/over,psym=10
cgplot,!x.crange,[0,0],linestyle=2,/over
;cgplot,[Uvrange[0],Uvrange[0]],!y.crange,linestyle=2,/over,color='blue'
;cgplot,[Uvrange[1],Uvrange[1]],!y.crange,linestyle=2,/over,color='blue'
;cgtext,mean(Uvrange),1.75,namelist[i],color='blue',alignment=0.5;,charsize=2.0

;TpeakU=max(specUa[coord2pix(hdrUa,L2vrange[0]*1000,3):coord2pix(hdrUa,L2vrange[1]*1000,3)])
;print,TpeakU
;Tex=5.532/alog(1+5.532/(TpeakU+0.819))
;print,'Tex:',Tex
;darea=((0.5/180.0)*(!pi/180)*1000.0*dists[i])^2
;area=total(covL)*darea

;cropfits,dathdrL,Lvrange,dim='v',/dataform,output=LLL
;ICO=total(LLL.dat)/(total(covL)*abs(delL)/1000.)
;NH2=4.6e13*5e5*ICO*Tex/exp(-5.3/Tex)
;MH2=1.0/6.3e19*NH2*area

;specLa
;specLacut=specLa[coord2pix(hdrLa,L2vrange[0]*1000,3):coord2pix(hdrLa,L2vrange[1]*1000,3)]
;vLcut=vL[coord2pix(hdrLa,L2vrange[0]*1000,3):coord2pix(hdrLa,L2vrange[1]*1000,3)];

;gfit=gaussfit(vLcut,specLacut,a,nterms=3)
;print,a[2]

;printf,out,name[i],a[2],MH2,Tex,format='(a8,f10.1,e10.2,f10.1)'

delvar,datUa,datLa,datL2a,dathdrU,dathdrL,dathdrL2

endfor
cgps_close

;free_lun,out

end