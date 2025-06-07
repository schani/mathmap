filter eqr_pan_ts (
image in,
float FoV: 15-320 (150),
float eye: 0-1.5 (1),
float pan: -180-180 (0),
float vsh: -1-1 (0))
#
# equirectangular to panini per Tom Sharpless's interpretation
# of the projection implemented in his OpenGL program "Panini",
# namely a rectilinear projection of a 3D cylindrical image.
# Gives images identical to Panini version 0.7, except without
# the black holes at zenith & nadir, smaller range of eye point
# positions, and slightly different limit on fov vs. eye pos.
#
# angular scale factors
Sppr = W / (2*pi); # source pixels/radian
d = eye + 1;
wfov = pi * min( FoV, 160 * d ) / 180; # radians
Drpp = 2*d*tan(wfov/(2*d)) / W;
# destination coordinates in radians
xr = x * Drpp; yr = (y - Y * vsh) * Drpp;
# project from dest to source
azi = d * atan( xr, d);
alt = atan( yr * (eye + cos(azi)), d );
# source coordinates in pixels
sx = Sppr*azi; sy = Sppr*alt;
# pan & interpolate
sx = sx + W*pan/360;
if sx > X then sx = sx - W end;
if sx < -X then sx = sx + W end;
in(xy:[sx, sy])
end 
