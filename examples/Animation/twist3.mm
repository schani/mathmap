
filter twist_three(
     image in,
     int Spread: 1-400 (1))

if (t<0.5) then
qt=2*t;
else
qt=2-2*t;
end;

spread=Spread;
ss=qt*(W+spread)-W/2;
se=ss-spread;
angle =pi/2+2*pi*(x-se)/spread;

if x>=se then
if x<ss then
if sign(y)*y>Y*sign(sin(angle))*sin(angle) then
rgba:[0,0,0,0]
else
ny=y/sin(angle);
in(xy:[x,ny])
end
else
in(xy:[x,y])
end
end

end 