function blindwalk,arr,pos,footprint,count,limit
;wait,0.0
sz=size(arr)
cc=count
x=pos[0]
y=pos[1]
all=[[x,y+1],[x-1,y],[x,y-1],[x+1,y]]
block=$
(all[0,0] eq -1 || all[0,0] eq 21 || all[1,0] eq -1 || all[1,0] eq 21 || (where(all[*,0] eq footprint)) ne -1 || arr[all[0,0]>0<(sz[1]-1),all[1,0]>0<(sz[2]-1)] lt limit) && $
(all[0,1] eq -1 || all[0,1] eq 21 || all[1,1] eq -1 || all[1,1] eq 21 || (where(all[*,1] eq footprint)) ne -1 || arr[all[0,1]>0<(sz[1]-1),all[1,1]>0<(sz[2]-1)] lt limit) && $
(all[0,2] eq -1 || all[0,2] eq 21 || all[1,2] eq -1 || all[1,2] eq 21 || (where(all[*,2] eq footprint)) ne -1 || arr[all[0,2]>0<(sz[1]-1),all[1,2]>0<(sz[2]-1)] lt limit) && $
(all[0,3] eq -1 || all[0,3] eq 21 || all[1,3] eq -1 || all[1,3] eq 21 || (where(all[*,3] eq footprint)) ne -1 || arr[all[0,3]>0<(sz[1]-1),all[1,3]>0<(sz[2]-1)] lt limit)
if block then begin
repeat begin
xx=(footprint[cc-1])[0]
yy=(footprint[cc-1])[1]
allf=[[xx,yy+1],[xx-1,yy],[xx,yy-1],[xx+1,yy]]
cc--
;print,'back!'
if cc eq 0 then begin
;print,'stop!!!!'
return,-1
endif
blockf=$
(allf[0,0] eq -1 || allf[0,0] eq 21 || allf[1,0] eq -1 || allf[1,0] eq 21 || (where(allf[*,0] eq footprint)) ne -1 || arr[allf[0,0]>0<(sz[1]-1),allf[1,0]>0<(sz[2]-1)] lt limit) && $
(allf[0,1] eq -1 || allf[0,1] eq 21 || allf[1,1] eq -1 || allf[1,1] eq 21 || (where(allf[*,1] eq footprint)) ne -1 || arr[allf[0,1]>0<(sz[1]-1),allf[1,1]>0<(sz[2]-1)] lt limit) && $
(allf[0,2] eq -1 || allf[0,2] eq 21 || allf[1,2] eq -1 || allf[1,2] eq 21 || (where(allf[*,2] eq footprint)) ne -1 || arr[allf[0,2]>0<(sz[1]-1),allf[1,2]>0<(sz[2]-1)] lt limit) && $
(allf[0,3] eq -1 || allf[0,3] eq 21 || allf[1,3] eq -1 || allf[1,3] eq 21 || (where(allf[*,3] eq footprint)) ne -1 || arr[allf[0,3]>0<(sz[1]-1),allf[1,3]>0<(sz[2]-1)] lt limit)
;print,blockf
endrep until blockf eq 0
repeat begin
one=fix(randomu(seed,1)*4)<3
noutf=$
(allf[0,one] eq -1 || allf[0,one] eq 21 || allf[1,one] eq -1 || allf[1,one] eq 21 || $
(where(allf[*,one] eq footprint)) ne -1 || arr[allf[0,one]>0<(sz[1]-1),allf[1,one]>0<(sz[2]-1)] lt limit)
;print,~repeatf
endrep until ~noutf
;print,'go again!'
return,allf[*,one]
endif else begin
repeat begin
one=fix(randomu(seed,1)*4)<3
nout=$
(all[0,one] eq -1 || all[0,one] eq 21 || all[1,one] eq -1 || all[1,one] eq 21 || $
(where(all[*,one] eq footprint)) ne -1 || arr[all[0,one]>0<(sz[1]-1),all[1,one]>0<(sz[2]-1)] lt limit)
endrep until ~nout
;print,'go straight!'
return,all[*,one]
endelse
end


function ccwalk,arr,pos,footprint,count,limit
;wait,0.05
sz=size(arr)
cc=count
x=pos[0]
y=pos[1]
all=[[x,y+1],[x-1,y],[x,y-1],[x+1,y]]
block=$
(all[0,0] eq -1 || all[0,0] eq 21 || all[1,0] eq -1 || all[1,0] eq 21 || (where(all[*,0] eq footprint))[0] ne -1 || arr[all[0,0]>0<(sz[1]-1),all[1,0]>0<(sz[2]-1)] lt limit) && $
(all[0,1] eq -1 || all[0,1] eq 21 || all[1,1] eq -1 || all[1,1] eq 21 || (where(all[*,1] eq footprint))[0] ne -1 || arr[all[0,1]>0<(sz[1]-1),all[1,1]>0<(sz[2]-1)] lt limit) && $
(all[0,2] eq -1 || all[0,2] eq 21 || all[1,2] eq -1 || all[1,2] eq 21 || (where(all[*,2] eq footprint))[0] ne -1 || arr[all[0,2]>0<(sz[1]-1),all[1,2]>0<(sz[2]-1)] lt limit) && $
(all[0,3] eq -1 || all[0,3] eq 21 || all[1,3] eq -1 || all[1,3] eq 21 || (where(all[*,3] eq footprint))[0] ne -1 || arr[all[0,3]>0<(sz[1]-1),all[1,3]>0<(sz[2]-1)] lt limit)
if block then begin
repeat begin
xx=(footprint[cc-1])[0]
yy=(footprint[cc-1])[1]
allf=[[xx,yy+1],[xx-1,yy],[xx,yy-1],[xx+1,yy]]
cc--
if cc eq 0 then begin
return,-1
endif
blockf=$
(allf[0,0] eq -1 || allf[0,0] eq 21 || allf[1,0] eq -1 || allf[1,0] eq 21 || (where(allf[*,0] eq footprint))[0] ne -1 || arr[allf[0,0]>0<(sz[1]-1),allf[1,0]>0<(sz[2]-1)] lt limit) && $
(allf[0,1] eq -1 || allf[0,1] eq 21 || allf[1,1] eq -1 || allf[1,1] eq 21 || (where(allf[*,1] eq footprint))[0] ne -1 || arr[allf[0,1]>0<(sz[1]-1),allf[1,1]>0<(sz[2]-1)] lt limit) && $
(allf[0,2] eq -1 || allf[0,2] eq 21 || allf[1,2] eq -1 || allf[1,2] eq 21 || (where(allf[*,2] eq footprint))[0] ne -1 || arr[allf[0,2]>0<(sz[1]-1),allf[1,2]>0<(sz[2]-1)] lt limit) && $
(allf[0,3] eq -1 || allf[0,3] eq 21 || allf[1,3] eq -1 || allf[1,3] eq 21 || (where(allf[*,3] eq footprint))[0] ne -1 || arr[allf[0,3]>0<(sz[1]-1),allf[1,3]>0<(sz[2]-1)] lt limit)
endrep until blockf eq 0
for i=0,3 do begin
noutf=$  
(allf[0,i] eq -1 || allf[0,i] eq 21 || allf[1,i] eq -1 || allf[1,i] eq 21 || $
(where(allf[*,i] eq footprint))[0] ne -1 || arr[allf[0,i]>0<(sz[1]-1),allf[1,i]>0<(sz[2]-1)] lt limit)
if noutf eq 0 then return,allf[*,i]
endfor
endif else begin
for i=0,3 do begin
nout=$  
(all[0,i] eq -1 || all[0,i] eq 21 || all[1,i] eq -1 || all[1,i] eq 21 || $
(where(all[*,i] eq footprint))[0] ne -1 || arr[all[0,i]>0<(sz[1]-1),all[1,i]>0<(sz[2]-1)] lt limit)
if nout eq 0 then return,all[*,i]
endfor
endelse
end


pro walkboy

arr=make_array(21,21,value=0.0)
arr[5,5]=100
arr[13,13]=30
arr[14,10]=40
arr[15,7]=50
arr=gauss_smooth(/EDGE_MIRROR,arr,1.5)

arr1=make_array(21,21,value=0.0)
;footprint=make_array(2,21*21,value=0)
footprint=list()
i0=15
j0=7

limit=1.0
wtime=0.0
cgdisplay,xsize=500,ysize=500
pos0=[0.05,0.05,0.95,0.95]
cgplot,[0],[0],/nodata,xrange=[-0.5,20.5],yrange=[-0.5,20.5],/aspect,position=pos0
cgloadct,0,clip=[64,191],/reverse
cgimage,bytscl(arr),position=pos0,/noerase
cgplot,[0],[0],/nodata,xrange=[-0.5,20.5],yrange=[-0.5,20.5],/aspect,position=pos0,/noerase
cgcontour,arr,/onimage

temp=[i0,j0]
cgplot,temp[0],temp[1],psym=1,/over,symsize=2,color='red'
count=0
time0=systime(/seconds)
while min(temp) ne -1 do begin
  footprint.add,temp
  ;help,temp
  arr1[temp[0],temp[1]]=1
  ;temp=blindwalk(arr,temp,footprint,count,limit)
  temp=ccwalk(arr,temp,footprint,count,limit)
  if min(temp) ne -1 then begin
  ;cgplot,[(footprint[count])[0],temp[0]],[(footprint[count])[1],temp[1]],/over,color='blue'
  ;cgarrow,/data,(footprint[count])[0],(footprint[count])[1],temp[0],temp[1],color='blue'
  cgplot,temp[0],temp[1],psym=1,/over,color='yellow'
  endif
  count++
endwhile
time1=systime(/seconds)
;print,footprint
print,time1-time0
cgcontour,arr1,/onimage,nlevels=2,label=0,color='red'
;cgimage,bytscl(arr1),position=pos0,/noerase

end