filter PVFflat( image in, # a Pannini view...
float hfov: 30-250 (150), # true angular width
float eyeDist: 0-2 (1), # projection parameter
float PCX: -0.2-0.2 (0), # pano center
float PCY: -0.2-0.2 (0),
float VPX: -0.3-0.3 (0), # vanishing point
float VPY: -0.3-0.3 (0),
float ULangl: 0-90 (45), # angles of the vee edges...
float LLangl: 0-90 (45),
float URangl: 0-90 (45),

float LRangl: 0-90 (45)
)

### Flattens upper and lower Vees in a Panini projection ###
# copyright (c) 2009 TKSharpless, all rights reserved #
# 4 vee edges intersect at the vanishing point, which is not #
# necessarily at the projection center, which need not be at #
# the center of the image. #
### ###


# vee valid flags
amin = 3.0; # minimum valid vee angle
uok = 90 - ULangl >= amin && URangl >= amin;
lok = 90 - LLangl >= amin && LRangl >= amin;

# slopes and normals of the vee lines
# normals point inside the vee
th = deg2rad(180 - ULangl);
st = sin(th); ct = cos(th);

uln = v2:[ st, -ct ];
uls = if uok then st / ct else 0 end;

th = deg2rad(90 - URangl);

st = sin(th); ct = cos(th);

urn = v2:[ -st, ct ];
urs = if uok then st / ct else 0 end;

th = deg2rad(-180 + LLangl);

st = sin(th); ct = cos(th);

lln = v2:[ -st, ct ];
lls = if lok then st / ct else 0 end;

th = deg2rad(-90 + LRangl);

st = sin(th); ct = cos(th);

lrn = v2:[ st, -ct ];
lrs = if lok then st / ct else 0 end;

# Pannini distance parameters
ed = eyeDist + 1; # eye to pic, pano radians
# eye distance from picture plane in pixels
edp = 0.5 * W / tan( 0.5 * deg2rad( hfov ) / ed );
# panosphere radius in pixels
Rp = edp / ed;

#image coordinates of PC and VP
pcx = PCX * W; pcy = PCY * H; # projection center
vpx = VPX * W; vpy = VPY * H; # vanishing point

# coords relative to projection center
# in eye radians = pano radians / ed
evx = (vpx - pcx) / edp; evy = (vpy - pcy) / edp;
ptx = (x - pcx) / edp; pty = (y - pcy) / edp;
# coords relative to VP
pdx = ptx - evx; pdy = pty - evy;
evec = v2:[ pdx, pdy ];

# slant of cross lines
slu = evx;

# select upper or lower vee
if pdy > 0 then
vok = uok;
vln = uln; vrn = urn;
vls = uls; vrs = urs;
slp = slu;

else
vok = lok;
vln = lln; vrn = lrn;
vls = lls; vrs = lrs;
slp = -slu;

end;

ty = y; # default remapped Y

# correct Y if this point is in a valid vee
if vok && dotp( evec, vln )> 0 && dotp( evec, vrn ) > 0 then
# intersections of line thru pt with vee lines (vp-org)
ldx = (pdy - slp * pdx )/ (vls - slp);
ldy = vls * ldx;

rdx = (pdy - slp * pdx) / (vrs - slp);
rdy = vrs * rdx;
# map img points to cylinder (pc-org)
lth = ed * atan( ldx + evx );
lcz = cos( lth );
lcy = (ldy + evy) * (ed - 1 + lcz) / ed;
lcx = sin( lth );
rth = ed * atan( rdx + evx );
rcz = cos( rth );
rcy = (rdy + evy) * (ed - 1 + rcz) / ed;
rcx = sin( rth );
pth = ed * atan( ptx );
ptz = cos( pth );
# pano plane on the intersection pts (unit normal)
ppn = normalize(crossp( v3:[rcx, rcy, rcz], v3:[lcx, lcy, lcz]));
# pano point on that plane at this point's azimuth
spt = crossp(ppn, v3:[ ptz, 0, -sin(pth) ] );
# map pano point y to image
pcy = spt[1] / abs(v2:[spt[0], spt[2]]); # cylinder y
piy = pcy * ed / ( ed - 1 + ptz );

ty = edp * piy;



end;


in(xy:[x,ty])

end 