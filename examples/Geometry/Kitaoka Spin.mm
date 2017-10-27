
#Kitaoka Spin Illusion V0.0
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
filter Kitty_Rings (image in, float sectors:10-80 (30), float zoom:1-30 (8),float inner_radius:0-2 (0), float outer_radius:0-2 (1), int R1:0-255 (113),int G1:0-255 (0),int B1:0-255 (113), int R2:0-255 (0),int G2:0-255 (255),int B2:0-255 (0))v=(sectors*a/(pi*2))+(floor(log(r)*zoom)*0.5); v=v-floor(v);out=rgbaColor(0,0,0,1);if inintv(r,inner_radius,outer_radius) then if inintv(v,0.15,0.5) then out=rgbaColor(R1/255,G1/255,B1/255,1);end;if inintv(v,0.5,0.65) then out=rgbaColor(1,1,1,1);end; if inintv(v,0.65,1) then out=rgbaColor(R2/255,G2/255,B2/255,1);end;end;out end