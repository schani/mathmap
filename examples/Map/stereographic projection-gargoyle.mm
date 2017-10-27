filter StereoNdeG3 (image in, float sc: 0-4 (1), color back)
rho=sc*r;
radius=Y;
if r>Y then
back;
else
maxpi=2*atan(sc);
colat=2*atan(rho/radius);
long=a;
ny=(Y)*(2*colat/maxpi)-(Y);
nx=(X-1)*long/pi-X;
in(xy:[nx,ny]);
end
end
