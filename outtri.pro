function outtri,x,y,size,posangle

n_points = posangle*!PI/180.-!PI/(6*10)*5+(!PI/(6*10)) * FINDGEN(11)
xs = x + size * COS(n_points)
ys = y + size * SIN(n_points)

return,TRANSPOSE([[x,xs,x],[y,ys,y]])
;cgcolorfill
;cgplots
end