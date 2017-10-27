##working stereo 1.13 inc zoom, twist, warp
## Original by Nathan de gargoyle, mangled by VMOS
filter StereoNdeG2 (image in, float turn: 0-1 (0.00), float zoom: 0-5
(1.00), float sc: 0-4 (1), float warp: 0-3.141592, color back)

zeta=-sin(warp)*1+cos(warp);
rho=sc*r;
radius=Y*zoom;
if r>radius then
back;
else
maxpi=2*atan(sc);
colat=2*atan(rho/radius);
long=(a+2*pi*turn)%(2*pi);
ny=((Y)*(2*colat/maxpi)-(Y));
nx=(X-1)*long/pi-X;
in(xy:[nx,ny*zeta]);
end
end 