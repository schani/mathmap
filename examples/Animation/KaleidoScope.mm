filter kaleidoscope1 (image in, int ng: 2-20, float rot: 0-6.28319, float sx:
0-1, float sy: 0-1, float rad: 0-100)
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