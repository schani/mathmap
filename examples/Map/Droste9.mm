######################################################################################################
# Droste effect code version 9 for Mathmap 1.2.0 #
# Originally by breic ( www.flickr.com/photos/breic ) #
# Additional Features and Mathmap 1.2 conversion by Josh Sommers (www.flickr.com/photos/joshsommers) #
# See also the flickr group: www.flickr.com/groups/escherdroste #
# Last modified: 6/10/2007 #
######################################################################################################

#Decription of User Values:
#InnerRadius: The percentage of the overall image that you want to be occupied by the spiral.
#OuterRadius: The percentage size of the overall image that you want to use in the effect.
#Periodicity: The number of times the image is repeated on each iteration of the spiral.
#Strands: The number of strands/arms the spiral will have.
#Zoom: Zoom in or out of the spiral.
#Rotate: Rotate the spiral. Positive numbers rotate clockwise.
#XShift: Shift the image on the X axis. Positive numbers shift to the right.
#YShift: Shift the image on the Y axis. Positive numbers shift upwards.
#XCenterShift: Shift the center of the spiral on the X axis. Positive numbers shift to the right.
#YCenterShift: Shift the center of the spiral on the Y axis. Positive numbers shift upwards.
#StartingLevel: The level that the spiral starts at. For internal transparency or no transparency, the first level is the outermost level. For outer transparency, the first level is the innermost level.
#NumberOfLevels: The total number of repetitions of the spiral.
#LevelFrequency: The frequency of levels. If set at 1, you will get every level. If set at 2 you will get every other level. If set at 3 you will get every third level, and so on.
#AutoSetPeriodicity: If set to true, the optimal value for Periodicity will be set for you.
#NoTransparency: Set to true if you do not wish to tile based upon a transparent region in your image.
#ExternalTransparency: Set the to true if the transparent region is on the outside of the image. The default is false, indicating that the transparent region is on the inside of the image.
#MirroEffect: Only works when using multiple strands. Set to true to mirror strands.
#Untwist: Set to true to see the image in log coordinates.
#DoNotFlattenTransparency: Set to true to leave semi-transparent pixels semi-transparent.
#Show Grid: The green grid shows each basic annulus, with (outer radius)/(inner radius) = r2/r1. The blue grid shows the green grid mapped into log coordinates (in which it truly is a grid), rotated, and exponentiated forward again. This allows visualization of the Droste transformation.
#ShowFrame: The frame is a black-and-white border in the standard view, and its preimage in the log coordinates. In log coordinates, the shaded region to the right of the frame will be mapped outside the viewport, while the region to the left will be visible.

############################################################
## You should not need to change anything below this line ##
############################################################

filter droste1 (
image in,
int InnerRadius: 1 - 100 (25),
int OuterRadius: 1 - 100 (100),
float Periodicity: -6 - 6 (1),
int Strands: -6 - 6 (1),
int Zoom: 1-100 (1),
int Rotate: -360-360 (0),
int XShift: -100 - 100 (0),
int YShift: -100 - 100 (0),
int XCenterShift: -100 - 100 (0),
int YCenterShift: -100 - 100 (0),
int StartingLevel: 1-20 (1),
int NumberOfLevels: 1-20 (10),
int LevelFrequency: 1-10 (1),
bool AutoSetPeriodicity,
bool NoTransparency,
bool ExternalTransparency,
bool MirrorEffect,
bool Untwist,
bool DoNotFlattenTransparency,
bool ShowGrid,
bool ShowFrame)


#Set code variables from user variables
r1= InnerRadius/100;
r2= OuterRadius/100;
p1= Periodicity;
p2= Strands;
xCenterShift = XCenterShift/100;
yCenterShift = YCenterShift/100;
xShift = (XShift*W/X)/100;
yShift = (YShift*H/Y)/100;
tileBasedOnTransparency = !(NoTransparency);
transparentPointsIn = !(ExternalTransparency);
levelsToLookOut=StartingLevel;
levelToShow=LevelFrequency;
retwist=!(Untwist);

if (AutoSetPeriodicity) then
p1= p2/2 * (1+sqrt(1-(log(r2/r1)/pi)^2));
end;

#Set Rotation
if p1 > 0 then
rotate=-(pi/180) * Rotate;
else
rotate=(pi/180) * Rotate;
end;

#Set Zoom
zoom=(Zoom+InnerRadius-1)/100;

epsilon=.01;

#######################
# Set up the viewport #
#######################
if (retwist) then
xbounds=[-r2,r2];
ybounds=[-r2,r2];
else
ybounds=[0,2.1*pi];
xbounds=[-log(r2/r1), log(r2/r1)];
end;

minDimension=min(W, H);
xymiddle=ri:[0.5*(xbounds[0]+xbounds[1]),0.5*(ybounds[0]+ybounds[1])];
xyrange=xy:[xbounds[1]-xbounds[0], ybounds[1]-ybounds[0]];
aspectRatio=W/H;
xyrange[0]=xyrange[1]*aspectRatio;
xbounds=[xymiddle[0]-0.5*xyrange[0],xymiddle[0]+0.5*xyrange[0]];
z=ri:[xbounds[0]+(xbounds[1]-xbounds[0])*(x+W/2)/W,ybounds[0]+(ybounds[1]-ybounds[0])*(y+H/2)/H];



# only allow for procedural zooming/scaling in the standard coordinates
if (retwist) then
zinitial=z;
z = z - ri:[xShift,yShift];
z=xymiddle+(z-xymiddle)/zoom*exp(-I*rotate);
else
zinitial=r1*exp(z); # save these coordinates for drawing a frame later
zinitial=zinitial*Zoom*exp(I*rotate);
end;

if (retwist) then
z2=log(z/r1);
else
z2 = z;
end;

##################################
# Droste-effect math starts here #
##################################

alpha=atan(p2/p1*log(r2/r1)/(2*pi));
f=cos(alpha);
beta=f*exp(I*alpha);

# the angle of rotation between adjacent annular levels
if (p2 > 0)
then angle = 2*pi*p1;
else
angle =-2*pi*p1;
end;

if MirrorEffect then
angle=angle/Strands;
end;

z=p1*z2/beta;
rotatedscaledlogz=z; # save these coordinates for drawing a grid later
logz=z2; # save these coordinates for drawing a grid later
z=r1*exp(z);

################################
# Droste-effect math ends here #
################################

if (tileBasedOnTransparency && levelsToLookOut > 0) then
if (!transparentPointsIn) then ratio=r2/r1*exp( I*angle); end;
if ( transparentPointsIn) then ratio=r1/r2*exp(-I*angle); end;
z = z * (ratio^levelsToLookOut)/1;
end;

colorSoFar=rgba:[0,0,0,0];
alphaRemaining=1;
ix=minDimension/2*(z[0]+xCenterShift);
iy=minDimension/2*(z[1]+yCenterShift);
iXY = xy:[ix,iy];

ColorOut=in(iXY);
colorSoFar = colorSoFar = colorSoFar + (ColorOut*(alpha(ColorOut)*alphaRemaining));
alphaRemaining=alphaRemaining*(1-alpha(ColorOut));
sign=0;

if (tileBasedOnTransparency) then
if ( transparentPointsIn && alphaRemaining > epsilon) then
sign=-1;
end;
if (!transparentPointsIn && alphaRemaining > epsilon) then
sign= 1;
end;
else
radius=sqrt(z[0]*z[0]+z[1]*z[1]);
if (radius < r1) then sign=-1; end;
if (radius > r2) then sign= 1; end;
end;

if (sign < 0) then
ratio=r2/r1*exp( I*angle);
end;

if (sign > 0) then
ratio=r1/r2*exp(-I*angle);
end;

if (levelToShow > 1) then
ratio = exp(log(ratio)*levelToShow);
end;

iteration=StartingLevel;
maxiteration=NumberOfLevels+StartingLevel-1;

while (sign != 0 && iteration < maxiteration) do
z2=z*ratio;
z=z2;
rotatedscaledlogz=rotatedscaledlogz+ri:[0,-sign*angle];
ix=minDimension/2*(z[0]+xCenterShift);
iy=minDimension/2*(z[1]+yCenterShift);
iXY = xy:[ix,iy];
sign=0;
if (tileBasedOnTransparency) then
ColorOut=in(iXY);
colorSoFar = colorSoFar + (ColorOut*(alpha(ColorOut)*alphaRemaining));
alphaRemaining=alphaRemaining*(1-alpha(ColorOut));
if ( transparentPointsIn && alphaRemaining > epsilon) then sign=-1; end;
if (!transparentPointsIn && alphaRemaining > epsilon) then sign= 1; end;
else
radius=sqrt(z[0]*z[0]+z[1]*z[1]);
colorSoFar=in(iXY);
if (radius < r1) then sign=-1; end;
if (radius > r2) then sign= 1; end;
end;
iteration=iteration+1;
end;

ColorOut=colorSoFar;

if (ShowGrid) then
gridz=xy:[(logz[0]+10*log(r2/r1))%log(r2/r1), (logz[1]+10*2*pi)%(2*pi)];

if (gridz[0] < epsilon || gridz[0] > (log(r2/r1)-epsilon) || gridz[1] < epsilon || gridz[1] > (2*pi-epsilon)) then
ColorOut=rgba:[0,1,0,1];
end;

gridz=xy:[(rotatedscaledlogz[0]+10*log(r2/r1))%log(r2/r1), (rotatedscaledlogz[1]+10*2*pi)%(2*pi)];

if (gridz[0] < epsilon || gridz[0] > (log(r2/r1)-epsilon) || gridz[1] < epsilon || gridz[1] > (2*pi-epsilon)) then ColorOut=rgba:[0,0,1,1];
end;
end;

if (ShowFrame) then
gridz=xy:[zinitial[0],zinitial[1]];
if (gridz[0] < (aspectRatio*r2) && gridz[0] > -(aspectRatio*r2) && gridz[1] < r2 && gridz[1] > -r2) then
dx=min((aspectRatio*r2)-gridz[0], gridz[0]+(aspectRatio*r2));
dy=min(r2-gridz[1], gridz[1]+r2);
if (dx < (4*epsilon) || dy < (4*epsilon)) then
ColorOut=rgba:[1,1,1,1];
end;
if (dx < (2*epsilon) || dy < (2*epsilon)) then
ColorOut=rgba:[0,0,0,1];
end;
else
ColorOut=rgba:[0.75*red(ColorOut),0.75*green(ColorOut),0.75*blue(ColorOut),1];
end;
end;

if !(DoNotFlattenTransparency) then
ColorOut=rgba:[ColorOut[0], ColorOut[1], ColorOut[2], 1];
end;

ColorOut

end 