################################################################
# Quincuncial code version 1 for MathMap
# By: breic ( www.flickr.com/photos/breic )
# Also see the flickr group:
# www.flickr.com/groups/mathmap
# Last modified: 4/19/2008
# minor modification by fpsurgeon (www.flickr.com/photos/fpsurgeon) on
# 05/06/2008 with addition of the 'flipOrient' and 'singleSquare' options.
# animated by Tas_mania June 2020
################################################################
# README:
# Quincuncial implements the Peirce quincuncial projection, a
# conformal mapping of the sphere onto a square. See
# en.wikipedia.org/wiki/Quincuncial_map
# for details of this conformal mapping. It also implements some
# of the trivial generalizations listed at
# www.progonos.com/furuti/MapProj/Normal/ProjConf/projConf....
#
# Parameters:
# * "in" must be an equirectangular projection of a full spherical
# panorama. Note that the aspect ratio must be exactly 2:1.
# * "imageH" must be set to the height of the input image.
# * "tiles" is the number of square tiles that will be displayed.
# * use "rotateSphere" and "equatorLevel" to adjust the position of
# the source spherical panorama.
# * check "twoHemispheres" to show both hemispheres in one square
# tile. Then "rotatePole" will adjust the position of the south
# pole (straight down) along the position of the tile boundary.
# There are some additional features just for fun:
# * click "DoubleQuinc" to apply the map twice in a row (with one
# hemisphere of the tiled plane mapped to a square).
# * click "Drostify" to apply an Escher's Droste effect to the tiles.
# When "Drostify" is turned on, the parameters "DrosteP1" and
# "DrosteP2" adjust its appearance. "DrosteP1" is the number of
# strands spiraling in, and "DrosteP2" is the number of repetitions
# of the basic element per cycle.
#
# Experiment with all the sliders. You shouldn't need to change
# any of the code below.
################################################################

filter animated_quincuncial_deluxe (
        image in,
	int imageH: 1 - 10000 (500),
	int tiles: 1 - 5 (3),
	float rotateSphere: -180 - 180 (0),
#	bool flipSphere,
	float equatorLevel: -90 - 90 (0),
	bool twoHemispheres,
	float rotatePole: 0 - 90 (0), # only has an effect when twoHemispheres is checked
	bool Drostify, # a limited, integrated Droste effect, for fun (of course you can also Pierce the Droste, etc...)
	int DrosteP1: 0 - 3 (1),
	int DrosteP2: 1 - 3 (1),
	bool DoubleQuinc,
	bool flipOrient,
	bool singleSquare
)

w = ri:[x, y];

if (!Drostify || DoubleQuinc) then
# translate so that the origin (0,0) is at lower-left, instead of in the center
## there appears to be a bug in the "unit" filter option
if (W < H) then
w = w + (W/(1.0*H)+I);
else
w = w + (1+H/(1.0*W)*I);
end;
end;

if (DoubleQuinc) then
w = w * 2.622057554292119810464839589891119413682754951431623162816821704;
w = w * (tiles / 2.0);
w = w * exp(I * pi / 4.0);
z = ri:[w[1], w[0]]; w = z; # flip orientation?
w = ell_jac_cn(w, 0.5);
end;

if (Drostify) then
w = log(w);
# the fundamental domains are different (b/c of orientation) depending on whether we are looking at one hemisphere per square or two
w = w / (2*pi) * 2;
w = w * 2;
w = w * (DrosteP1 + DrosteP2*I) / I;
end;

w = w * 2.622057554292119810464839589891119413682754951431623162816821704;
if (!Drostify && !DoubleQuinc) then
w = w * (tiles / 2.0);
else
w = w / 2.0;
end;

if (singleSquare) then
w = w; # no need to rotate the imaginary plane.
z = ri:[w[0]/tiles/sqrt(2)*2, w[1]/tiles/sqrt(2)*2-(log(2*pi))]; # I don't know if this is totally correct.
w = z;
else
w = w * exp(I * pi / 4.0); # rotates the imaginary plane 45 degrees.
end;


if (flipOrient) then
z = ri:[w[1], w[0]]; w = z; # flip orientation?
else
z = ri:[w[0], w[1]]; w = z;
end;
# this is the basic operation; it maps the square with corners at (0,0),
# (c,-c), (2c,0), (c, c), to the unit circle (angles 0, pi/2, pi, 3*pi/2,
# respectively---note that the orientation is preserved), where
# c = EllipticK(1/2) = 2.62205755...
# just before this step, we rotated by pi/4 so that the square is axis-aligned
w = ell_jac_cn(w, 0.5);

# inverse stereographic projection
if (twoHemispheres) then
w = exp(rotatePole * pi / 180 * I) * w;
#wr = w[0]*w[0] + w[1]*w[1];
#p = v3:[2*w[0] / (1 + wr), 2*w[1] / (1 + wr), (-1 + wr) / (1 + wr)];
#q = v3:[-p[2], p[1], p[0]]; p = q; # Mathmap bug
## stereographic projection
#w = ri:[p[0] / (1 - p[2]), p[1] / (1 - p[2])];
w = (-1 - w) / (-1 + w); # this code has the same effect as the above five lines
w = w * w;
end;

# scale the stereographic projection to move the equator level up or down
w = w * tan(pi/4. - equatorLevel * pi / 180. / 2.);

# inverse stereographic projection,
wr = w[0]*w[0] + w[1]*w[1];
p = v3:[2*w[0] / (1 + wr), 2*w[1] / (1 + wr), (-1 + wr) / (1 + wr)];

# convert to latitude/longitude (can cause aliasing errors where longitude wraps around)
long = (atan(p[1], p[0]) + pi) * 180 / pi;
long = (long - rotateSphere + 2*360*t) % 360;
lat = asin(p[2]) * 180 / pi;
# if flipSphere, should probably also rotate in the opposite direction
#if (flipSphere) then
# long = 360 - long;
# lat = -lat;
#end;

imageW = imageH * 2;

imagex = (imageW * long / 360 + imageW/2) % imageW - imageW/2;
imagey = lat / 180 * imageH;
# make height vary in [0,imageH-1] instead of [0,imageH]
imagex = imagex * (imageW-1) / (1. * imageW);
imagey = imagey * (imageH-1) / (1. * imageH);
colorOut = in(xy:[imagex, imagey]);

colorOut
end 
