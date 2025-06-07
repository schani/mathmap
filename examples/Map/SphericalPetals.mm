################################################################
# SphericalPetals code version 1 for MathMap
# By: breic ( www.flickr.com/photos/breic )
# Also see the flickr group: 
#  www.flickr.com/groups/mathmap
# Last modified: 3/29/2008
################################################################
# README:
# SphericalPetals gives a conformal mapping of the sphere onto 
# a flower shape.  
#  * "in" must be an equirectangular projection of a full spherical panorama. 
#      Note that the aspect ratio must be exactly 2:1.  
#  * imageH must be set to the height of the input image.
#  * numPetals is the number of petals the flower will have.
#  * petalSize is the ratio between the length of a petal and the distance at 
#      which it meets its neighbor; petalSize = 1 will give a circle.
#  * use rotate, scaleFactor and flipImage to adjust the position of the 
#     spherical panorama.  With a scaleFactor of 1, the equator of the input 
#     touches the point where two petals meet.
# Experiment with all the sliders.  You shouldn't need to change any of the 
# code below.  
# This technique is inspired by lloydb's photo at 
#   http://www.flickr.com/photos/lloydb/2099230962/in/photostream/ .  Thanks, 
# lloydb, for describing how it works. 
# Please report to me any bugs you find.  Currently the only known bug is that 
# it renders a gap along the meridian when antialiasing or supersampling is 
# turned on; a workaround is to render it twice, once with these both turned 
# off, in order to cover the gap.
################################################################

filter SphericalPetals (
	image in, 
	int imageH: 1 - 10000 (500),
	int numPetals: 1 - 10 (5),
	float petalSize: .25 - 2.5 (1.5),
	float rotate: 0 - 360 (0),
	float scaleFactor: 0.33 - 3 (1), 
	bool flipImage, 
	color background
)

true = 1;
false = 0;

imageW = imageH * 2;

#power = 2 - petalSize;
power = (pi/2) / atan(petalSize ^ (numPetals / 2.0));
sf = 1.01; # slightly larger than 1 so there is small margin always
if (power < 2) then 
#	sf = sf * tan(pi / (2*power)) ^ (2.0 / numPetals);
	sf = sf * petalSize;
end;
w = ri:[x, y];
w = w * (2/min(W, H)) * sf;

angle = atan(w[1], w[0]);
angle = (angle + 2*pi) % (2 * pi); # range(atan) is (-pi, pi], shift this to [0, 2*pi)
petalNum = floor( angle / (2*pi / numPetals) );
w = w * exp(-I * petalNum * (2 * pi / numPetals));

# we have a slice from angle 0 to angle pi/(numPetals/2.0); 
# stretch this slice over to angle pi
w = w ^ (numPetals / 2.0);

# inverse stereographic projection, 
# rotation so (1,0,0) points up to (0,0,1), 
# pull the petal so it reaches the north pole (by raising it to power), 
# rotate back
# stereographic projection
wr = w[0]*w[0] + w[1]*w[1];
p = v3:[2*w[0] / (1 + wr), 2*w[1] / (1 + wr), (-1 + wr) / (1 + wr)];
q = v3:[-p[2], p[1], p[0]]; p = q; # MathMap bug: must use different variable on LHS
w = ri:[p[0] / (1 - p[2]), p[1] / (1 - p[2])];
outsideFlower = false;
if (atan(w[1], w[0]) > pi/power) then 
	outsideFlower = true;
end;
w = w ^ power;
wr = w[0]*w[0] + w[1]*w[1];
p = v3:[2*w[0] / (1 + wr), 2*w[1] / (1 + wr), (-1 + wr) / (1 + wr)];
q = v3:[p[2], p[1], -p[0]]; p = q;
w = ri:[p[0] / (1 - p[2]), p[1] / (1 - p[2])];

# undo the petal stretching
w = w ^ (2.0 / numPetals);

# move back to proper angular position
w = w * exp(I * petalNum * (2 * pi / numPetals));

# scale about 0, and inverse stereographic projection
w = w / scaleFactor;
wr = w[0]*w[0] + w[1]*w[1];
p = v3:[2*w[0] / (1 + wr), 2*w[1] / (1 + wr), (-1 + wr) / (1 + wr)];

# convert to lat/long
long = (atan(p[1], p[0]) + pi) * 180 / pi;
long = (long - rotate + 2*360) % 360;
lat = asin(p[2]) * 180 / pi;
if (flipImage) then
	long = 360 - long;
	lat = -lat;
end;

colorOut = in(xy:[(imageW * long / 360 + imageW/2) % imageW - imageW/2, imageH * lat / 180]);
if (outsideFlower) then 
	colorOut = background;
end;

colorOut
end
