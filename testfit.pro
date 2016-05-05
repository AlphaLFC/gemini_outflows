pro testfit
xx=indgen(10)+5.0
yy=indgen(10)+5.0
xperr=randomu(seed,10)*0.5+0.2
xnerr=randomu(seed,10)*0.5+0.2
yperr=randomu(seed,10)*0.75+0.5
ynerr=randomu(seed,10)*0.75+0.5
;cgplot
cgplot,xx,yy,err_xhigh=xperr,err_xlow=xnerr,err_yhigh=yperr,err_ylow=ynerr,psym=1,color='black',xrange=[4,15],yrange=[-2,22]
for i=0,999 do begin
choose=round(randomu(seed,10))
xperr0=xperr*abs(randomn(seed,10))*choose
xnerr0=-xnerr*abs(randomn(seed,10))*(~choose)
xerr0=xperr0+xnerr0
choose=round(randomu(seed,10))
yperr0=yperr*abs(randomn(seed,10))*choose
ynerr0=-ynerr*abs(randomn(seed,10))*(~choose)
yerr0=yperr0+ynerr0

xx0=xx+xerr0
yy0=yy+yerr0
plotlinfit,/over,xx0,yy0,psym=1,color='red',/nodat
endfor
plotlinfit,xx,yy,psym=1,fcolor='black',/over
cgplot,xx,yy,err_xhigh=xperr,err_xlow=xnerr,err_yhigh=yperr,err_ylow=ynerr,psym=1,color='black',/over
end