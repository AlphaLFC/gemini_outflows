pro determine_outflows

if ~keyword_set(region) then region='swallow'
if ~keyword_set(Lvrange) then Lvrange=[12,18]
cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/'+region+'/candidates'
distance=3.8

readcol,'outflowcat.cat',num,glon,glat,wb0,wb1,wr0,wr1,tag,bnum,rnum,format='I,F,F,F,F,F,F,A,I,I',stringskip='#'
readcol,'manu_para.cat',nump,szx,szy,conB,conR,format='I,F,F,F,F',stringskip='#'
openw,cat,/get_lun,'derived_para.cat'
if n_elements(num) eq n_elements(nump) then begin
for i=0, n_elements(num)-1 do begin
  if tag[i] eq 'D' then begin
    
  endif
  
endfor
endif
end