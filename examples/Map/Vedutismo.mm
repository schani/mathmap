filter panini_FX (image in, float FoV: 1-360 (230.07), float pan: -180-180 (0.0))
# Vedutismo / Panini effect
# Bruno Postle December 2008
# input is any 360 cylindrical image
maxphi=atan(pi/2);
newphi=FoV*pi/360/2;
scale=tan(newphi)/tan(maxphi);
phi=atan(x*scale*pi/W);
yscale=cos(phi)*cos(phi);
in(xy:[phi/pi*W+(W*pan/360),y*scale*yscale])
end 