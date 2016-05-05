pro testcmpdat

cd,'/home/Alpha/Astrodata/galaxysurvey/-18_30/out_search_scripts/'
readcol,'redpeaks.cat',rsn,rpeak1,rpeak2,rscen1,rscen2,rsmaj,rsmin,rsposang,rsize1,rsize2,rpeak,$
  format='I,F,F,F,F,F,F,F,F,F,F',stringskip='#',/silent
readcol,'bluepeaks.cat',bsn,bpeak1,bpeak2,bscen1,bscen2,bsmaj,bsmin,bsposang,bsize1,bsize2,bpeak,$
  format='I,F,F,F,F,F,F,F,F,F,F',stringskip='#',/silent

newbpeak1=make_array(n_elements(bsn),value=999.)
newbpeak2=make_array(n_elements(bsn),value=999.)

newrpeak1=make_array(n_elements(rsn),value=999.)
newrpeak2=make_array(n_elements(rsn),value=999.)

newpeak1=make_array(n_elements(rsn)+n_elements(bsn),value=999.)
newpeak2=make_array(n_elements(rsn)+n_elements(bsn),value=999.)
;newlen=make_array(n_elements(rsn)+n_elements(bsn),value=999.)
bc=0
rc=0
count=0
;rpeak1[i],rpeak2,rscen1,rscen2
for i=0,n_elements(rsn)-1 do begin
  mpos=where(abs(rpeak1[where(rpeak1 ne rpeak1[i])]-rpeak1[i]) le 2./60 and $
    abs(rpeak2[where(rpeak2 ne rpeak2[i])]-rpeak2[i]) le 2./60 )  
  if mpos[0] ne -1 then begin
    newpos=rc++
    newrpeak1[newpos]=mean(bpeak1[mpos])
    newrpeak2[newpos]=mean(bpeak2[mpos])
  endif
endfor

;    if min(abs(bpeak1-rpeak1[i])) le 2./60 && min(abs(bpeak2-rpeak2[i])) le 2./60 then begin
;      newpos=count++
;      mpos1=where(abs(bpeak1-rpeak1[i]) eq min(abs(bpeak1-rpeak1[i])))
;      mpos2=where(abs(bpeak2-rpeak2[i]) eq min(abs(bpeak2-rpeak2[i])))
;      newpeak1[newpos]=(bpeak1[mpos1]+rpeak1[i])/2.
;      newpeak2[newpos]=(bpeak2[mpos1]+rpeak2[i])/2.
;      ;newlen[newpos]=
;    endif

newpos1=newpeak1[where(newpeak1 ne 999.)]
newpos2=newpeak2[where(newpeak2 ne 999.)]

openw,newcat,/get_lun,'newpos.cat'
for i=0, n_elements(newpos1)-1 do printf,newcat,newpos1[i],newpos2[i],format='(2f9.3)'
free_lun,newcat

end