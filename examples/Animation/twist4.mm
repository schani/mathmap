filter twist_four(image in)

if (t<0.5)
then qt=t;
sn=1;
else
qt=t-0.5;
sn=-1;
end;

ss=4*qt*W-W/2;
se=ss-W;
angle =pi/2+pi*(x-se)/W;

if x>=se then
if x<ss then
if sign(y)*y>Y*sign(sin(angle))*sin(angle) then
rgba:[0,0,0,0]
else
ny=-sn*y/sin(angle);
in(xy:[x,ny])
end
else
ny=sn*y;
in(xy:[x,ny])
end
else
ny=-sn*y;
in(xy:[x,ny])
end

end 