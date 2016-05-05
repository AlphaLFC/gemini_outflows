pro cal_area,img,lmt=lmt,area=area,mask=mask,low=low;,shape=shape
sz=size(img)
x=round(sz[1]/2.)-1
y=round(sz[2]/2.)-1
maski=img
if ~keyword_set(lmt) then lmt=0.3
area=1
for i=0.95, lmt, -0.05 do begin
  limit = i*mean(img[(x-1):(x+1),(y-1):(y+1)])
  maski[where(img gt limit)]=1
  maski[where(img le limit)]=0
  lab=label_region(maski)
  tag=lab[x,y]
  maski[where(lab ne tag)]=0
  temp=total(maski)
  ;print,i,temp
  ;print,i,':',(sqrt(temp)-sqrt(area)),temp
  if (sqrt(temp)-sqrt(area))/0.05 gt 60 then begin
    if area lt 50 then area=round(area+4*sqrt(area)+4.)>50.
    if i eq 0.95 then low=i else break
  endif
  area=temp
  mask=maski
  low=i
endfor
end