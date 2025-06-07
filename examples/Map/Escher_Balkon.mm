filter Balkon (image in, float rmax:0-1 (0.5),float p1:0-100 (40),float p2:0-20 (5),float p3:0-10 (5.5), float p4:0-10 (1.57))
# Searching Escher's Balkon transform

r1=r/H;
rprime=r1;
aprime=a;
if(r1<rmax) then
rprime=-(-p2+sqrt(p2+p1/2-p1*r1))/p3*cos(p4*r1)
end;
in(ra:[rprime*H,aprime])
end
