# Modified version of original Moire
filter moire3
(
float zoomR: 1-10000 (100),
float zoomG: 1-10000 (90),
float zoomB: 1-10000 (80))
   rgbColor(sin(x*y*zoomR+t*2*pi)*0.5+0.5,sin(x*y*zoomG+t*2*pi)
*0.5+0.5,sin(x*y*zoomB+t*2*pi)*0.5+0.5)
end 