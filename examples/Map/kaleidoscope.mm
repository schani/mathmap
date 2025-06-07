filter kaleidoscope (image in, int ng: 2-10, float rot: 0-6.28319, int
sx: 0-2000, int sy: 0-2000, float rad: 0-100 (1.0))

intang=2*pi/ng;
na=a+pi/2;
ang=intang/2-na%intang;
ang=rot+(intang/2-sqrt(ang^2));
xr=r*rad;
foo=min(X,Y);
bar=foo*rad;
if xr<bar then

nr=xr;
else
ang=2*ang;
nr=bar*xr/R;

end;
nx=nr*cos(ang);
ny=nr*sin(ang);
nx=(nx+sx)%(W-1)-(X-1);
ny=(ny+sy)%(H-1)-(Y-1);
in(xy:[nx,ny])
end