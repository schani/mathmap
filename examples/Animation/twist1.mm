filter twist_one(
     image in,
     int Twists: 1-8 (1))

twist=Twists;

angle = 2*pi*t+twist*pi*x/W;

if sign(y)*y>Y*sign(sin(angle))*sin(angle) then
rgba:[0,0,0,0]
else
ny=y/sin(angle);
in(xy:[x,ny]);
end

end 