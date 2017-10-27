filter r2e (image in)
borne=4*(x*x/(W*W)+(y*y)/(H*H));
xprime=x;
yprime=y;
if(1>=borne) then
if(W>H) then
xmax=sqrt(1-(4*y*y/(H*H)));
xprime=x/xmax;
else
ymax=sqrt(1-(4*x*x/(W*W)));
yprime=y/ymax;
end;
end;
if(borne>1) then
xprime=20;
yprime=20;
rgbColor(1,1,1)
else
in(xy:[xprime,yprime])
#rgbColor(0,1,1)
end
end 
