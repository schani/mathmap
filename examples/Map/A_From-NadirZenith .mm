
filter FromNadirZenith (image in)
# Filter created by Seb Przd
# Licensed under the GPL

output=1;
if y>Y/4 then
sinphi1=1;
xc=-X/2;
else if y<-Y/4 then
sinphi1=-1;
xc=X/2;
else
 output=0;
end;
end; 

cosc=sinphi1*sin(y/Y*pi/2);
xx=cos(y/Y*pi/2)*sin(x/X*pi)/cosc;
yy=-sinphi1*cos(y/Y*pi/2)*cos(x/X*pi)/cosc; 

if abs(xx)>1 then output=0; end;

if output then
in(xy:[xx*X/2+xc,yy*Y])
else
rgbaColor(0,0,0,0)
end

end
