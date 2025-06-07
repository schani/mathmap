#Kitaoka Spin Illusion V0.1
#Written by step7 (2012)
#Renders positional displacement illusion "Spinning Sectors"
#based on Akiyoshi Kitaoka's "Rotating Snakes"
#
#Wiring:
#None
#
#User Values:
#Just play..
#
#Contact me about the filter on Gimp Chat Forum or Mathmap Google Group
#

filter KittyRings (image in, float sectors:10-80 (30), float zoom:1-30 (8),float inner_radius:0-2 (0), float outer_radius:0-2 (1), color colour1, color colour2)v=(sectors*a/(pi*2))+(floor(log(r)*zoom)*0.5); v=v-floor(v);out=rgbaColor(0,0,0,1);if inintv(r,inner_radius,outer_radius) then if inintv(v,0.15,0.5) then out=colour1;end;if inintv(v,0.5,0.65) then out=rgbaColor(1,1,1,1);end; if inintv(v,0.65,1) then out=colour2;end;end;out end