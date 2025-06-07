filter ToNadirZenith (image in)
# Filter created by Seb Przd
# Licensed under the GPL

if x<0 then
sinphi1=1;
xx=x+X/2;
else
sinphi1=-1;
xx=x-X/2;
end;

yy=y;

rr=sqrt(xx^2+yy^2);

c=atan(rr/Y);

phi =
if rr == 0 then
 0
else
 asin(cos(c)*sinphi1)
end;

xxx=atan(xx,-yy*sinphi1)*X/pi;
yyy=phi*Y/(pi/2);

in(xy:[xxx,yyy])
end
