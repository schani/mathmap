################################################################
# ScaleEquirectangular code version 1 for MathMap
# By: breic ( www.flickr.com/photos/breic )
# Also see the flickr group: 
#  www.flickr.com/groups/equirectangular
# Last modified: 3/26/2008
# modified by Tas_Mania May 2020
################################################################
# README:
# ScaleEquirectangular is designed to automate and provide a friendly 
# interface for some common conformal transformations of full spherical 
# panoramas.  It allows for scaling in or out from a point that is chosen 
# by longitude and latitude.  When the "hideGuides" option is unchecked, 
# it shows several guides to help choose the right transformation.  The 
# red circle is centered on the point into which it zooms.  The "scaleFactor" 
# parameter determines how much to zoom in.  The pinkish-grey circle shows 
# the circle into which the red circle is transformed.  The next black circle 
# shows the equator, 90 degrees from the red circle.  Finally, there is also 
# a black circle opposite the red circle.  
#  * "in" must be an equirectangular projection of a full spherical panorama. 
#      Note that the aspect ratio must be exactly 2:1.  
#  * imageH must be set to the height of the input image.
#  * Check "showUntransformedImage" to revert temporarily to the original image.  
#  * Note that setting latitude to -90 or +90, the scaling will be into or out 
#     of the south and north poles (i.e., straight down and straight up, 
#     respectively, in the spherical panorama).  In this case, scaleFactors 
#     other than 1 will shift the horizon.
#  * Check "stereographicProjection" to see a stereographic projection of the 
#      image.  When this is checked, the "stereographicFOV" slider will 
#      determine the diagonal field-of-view of the stereographic projection.  
# Experiment with all the sliders.  You shouldn't need to change any of the 
# code below.  
# Please report to me any bugs you find.  Currently the only known bug is that 
# it renders a gap along the meridian when antialiasing or supersampling is 
# turned on; a workaround is to render it twice, once with these both turned 
# off, in order to cover the gap.
################################################################

filter animated_scale_equirectangular (
image in, 
int imageH: 1 - 10000 (500),
float scaleFactor: 0.25 - 4 (1), 
int longitude: 0 - 360 (180), 
int latitude: -90 - 90 (0),
bool hideGuides, 
bool showUntransformedImage,
bool stereographicOutput,
int stereographicFOV: 1 - 359 (180)
)

true = 1;
false = 0;

imageW = imageH * 2;

if (!stereographicOutput) then
 if (X < 2*Y) then 
  sf = W * 1.0 / imageW;
  xx = x / sf;
  yy = y / sf;
  long = 360 * x / W + 180;
  lat = 180 * y / (0.5 * W);
 end;
 if (X >= 2*Y) then 
  sf = H * 1.0 / imageH;
  xx = x / sf;
  yy = y / sf;
  long = 360 * x / (2 * H) + 180;
  lat = 180 * y / H;
 end;
 long = long * pi / 180;
 lat = lat * pi / 180;
 p = v3:[cos(long) * cos(lat), sin(long) * cos(lat), sin(lat)];
end;

if (stereographicOutput) then 
 sf = 2 * tan((stereographicFOV / 4) * pi / 180) / W;
 xx = x * sf;
 yy = y * sf;
 r2 = xx*xx + yy*yy;
 r2 = r * r * sf * sf;
 p = v3:[2*xx / (1 + r2), 2*yy / (1 + r2), (-1 + r2) / (1 + r2)];
 long = (atan(p[1], p[0]) + pi) * 180 / pi;
# long = a * 180 / pi;
 lat = asin(p[2]) * 180 / pi;
 xx = (imageW * long / 360 + imageW/2) % imageW - imageW/2;
 yy = imageH * lat / 180;
end;

colorOut = in(xy:[xx,yy]);  # color from the original, unscaled image

# this is the center on the sphere of the target point into which we scale
q = v3:[cos(longitude * 2 * pi / 360) * cos(latitude * pi / 180), sin(longitude * 2 * pi / 360) * cos(latitude * pi / 180), sin(latitude * pi / 180)];

if (!hideGuides) then
targetCircleInnerAngularRadius = [acos(0.995), pi-acos(0.995), pi/2];
scaledTargetCircleInnerAngularRadius = [0,0,0];
scaledTargetCircleInnerAngularRadius[0] = 2*atan(tan(targetCircleInnerAngularRadius[0]/2) * scaleFactor);
scaledTargetCircleInnerAngularRadius[1] = 2*atan(tan(targetCircleInnerAngularRadius[1]/2) * scaleFactor);
scaledTargetCircleInnerAngularRadius[2] = 2*atan(tan(targetCircleInnerAngularRadius[2]/2) * scaleFactor);
targetCircleAngularWidth = .01111111;

radius = acos(dotp(p, q));
inCircle = false;
if (radius < scaledTargetCircleInnerAngularRadius[0] && radius > scaledTargetCircleInnerAngularRadius[0] - targetCircleAngularWidth) then 
inCircle = true;
circleColor = rgba:[.6,.5,.5,1];
end;
if (radius < scaledTargetCircleInnerAngularRadius[1] && radius > scaledTargetCircleInnerAngularRadius[1] - targetCircleAngularWidth) then 
inCircle = true;
circleColor = rgba:[.5,.5,.6,1];
end;
if (radius < scaledTargetCircleInnerAngularRadius[2] && radius > scaledTargetCircleInnerAngularRadius[2] - targetCircleAngularWidth) then 
inCircle = true;
circleColor = rgba:[.5,.5,.5,1];
end;
if (radius < targetCircleInnerAngularRadius[2] && radius > targetCircleInnerAngularRadius[2] - targetCircleAngularWidth) then 
inCircle = true;
circleColor = rgba:[0,0,0,1];
end;
if (radius < targetCircleInnerAngularRadius[1] && radius > targetCircleInnerAngularRadius[1] - targetCircleAngularWidth) then 
inCircle = true;
circleColor = rgba:[0,0,.25,1];
end;
if (radius < targetCircleInnerAngularRadius[0] && radius > targetCircleInnerAngularRadius[0] - targetCircleAngularWidth) then 
inCircle = true;
circleColor = rgba:[1,0,0,1];
end;
end;

# rotate sphere
qr = sqrt(q[0]*q[0] + q[1]*q[1]);
rotation = m3x3:[q[1]/qr, q[0]*q[2]/qr, -q[0], -q[0]/qr, q[1]*q[2]/qr, -q[1], 0, -qr, -q[2]];
rotationinv = m3x3:[q[1]/qr,-q[0]/qr, 0, q[0]*q[2]/qr, q[1]*q[2]/qr, -qr, -q[0], -q[1], -q[2]];
p = rotationinv * p;

# scale about 0, and inverse stereographic projection
w1 = p[0] / (1 - p[2]);
w2 = p[1] / (1 - p[2]);
w1 = w1 / scaleFactor;
w2 = w2 / scaleFactor;
wr = w1*w1 + w2*w2;
p = v3:[2*w1 / (1 + wr), 2*w2 / (1 + wr), (-1 + wr) / (1 + wr)];

# inverse rotate sphere
p = rotation * p;

# This is another way of scaling.  It translates the complex plane based on a stereographic 
# projection from the South pole.  Therefore, the South pole (straight down) is fixed.  I have
# disabled it, since I think you usually get better results by scaling into a point on the 
# horizon (which keeps the horizon flat).
# float translateFactor: -1 - 1 (0)
#w1 = p[0] / (1 + p[2]);
#w2 = p[1] / (1 + p[2]);
#w1 = w1 + translateFactor * cos(longitude * pi / 180);
#w2 = w2 + translateFactor * sin(longitude * pi / 180);
#wr = w1*w1 + w2*w2;
#p = v3:[2*w1 / (1 + wr), 2*w2 / (1 + wr), -(-1 + wr) / (1 + wr)];

# convert to lat/long
long = (atan(p[1], p[0]) + pi) * 180 / pi;
lat = asin(p[2]) * 180 / pi;
if (!showUntransformedImage) then
colorOut = in(xy:[(imageW * long / 360 + imageW/2*t*2) % imageW - imageW/2, imageH * lat / 180]);
end;

if (!hideGuides && inCircle) then 
colorOut = circleColor;
end;

colorOut
end
