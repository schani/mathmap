filter PVSqeeze( image in,  # a Pannini view...
 float hfov: 30-250 (150),    #   true angular width
 float eyeDist: 0-2 (1),      #   projection parameter
 float PCX: -0.25-0.25 (0),  # pano center 
 float PCY: -0.25-0.25 (0),
 float VPX: -0.25-0.25 (0),  # vanishing point
 float VPY: -0.20-0.20 (0),
 float ULangl: 0-90 (45),  # angles of the vee edges...
 float LLangl: 0-90 (45),
 float URangl: 0-90 (45),
 float LRangl: 0-90 (45),
 float UTwid: 0-0.5 (0.25), # width of transition zone
 float UVlim: 0-0.5 (0),    # upr edge of pure vertical zone
 float LVlim: 0-0.5 (0),    # lwr edge of pure vertical zone
 float LTwid: 0-0.5 (0.25)  # width of transition zone
)
###  Flattens upper and lower Vees in a Pannini projection ###
#    by a mixed vertical/radial contraction                  #
# 4 vee edges intersect at the vanishing point, which is not #
# necessarily at the projection center, which need not be at #
# the center of the image.                                   #
# copyright (c) 2009 TKSharpless  Anyone is hereby licensed  #
# to use PVFlat in any way, but with no implied warranty     #
###                                                        ###

# vee valid flags
amin = 3.0;    # minimum valid vee angle
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

# Pannini projection parameters
ed = eyeDist + 1;     # eye to img in pano radians
# eye to image plane in pixels
edp = 0.5 * W / tan( 0.5 * deg2rad( hfov ) / ed );
# panosphere radius in pixels
Rp = edp / ed;

# image coordinates of projection center
pcx = PCX * W; 
pcy = PCY * H;

# vanishing point (PC-origin, eye radians)
vpx = (VPX - PCX) * W / edp;	
vpy = (VPY - PCY) * H / edp;

# vertical/radial Y limits
vuy = UVlim * H / edp - vpy;
ruy = vuy + UTwid * H / edp - vpy ;
vly = LVlim * H / edp - vpy;
rly = vly + LTwid * H / edp - vpy ;

# panocylinder coordinates of VP (pano radians)
pth = ed * atan( vpx );  # azimuth at eye
ptz = cos( pth );
pty = vpy * (ed - 1 + ptz);
ptx = sin( pth );
vpcyl = v3:[ ptx, pty, ptz ];

# current point (PC-origin, eye radians)
ptx = (x - pcx) / edp; 
pty = (y - pcy) / edp;
# ditto, VP-origin
pdx = ptx - vpx; pdy = pty - vpy;
evec = v2:[ pdx, pdy ];  # for in-vee test

# select upper or lower vee, or no corr
if pdy > 0 then 
  vok = if uok then
    dotp( evec, uln ) > 0 && dotp( evec, urn ) > 0
  else 0 end;
  vls = uls; vrs = urs; 
  vsgn = 1;
  vy = vuy; ry = ruy;
else 
  vok = if lok then
    dotp( evec, lln ) > 0 && dotp( evec, lrn ) > 0
  else 0 end;
  vls = lls; vrs = lrs;
  vsgn = -1;
  vy = vly; ry = rly;
end;

# default remapped point 
tx = x; ty = y;	    

# squeeze if this point is in a valid vee
if vok then 
  # intersections of line thru pt with vee lines
  # (VP-origin img coords in eye radians)
  slp = vsgn * vpx;
  ldx = (pdy - slp * pdx )/ (vls - slp);
  ldy = vls * ldx;
  rdx = (pdy - slp * pdx) / (vrs - slp);
  rdy = vrs * rdx;
  # shift from VP-origin to PC-origin
  lex = ldx + vpx; ley = ldy + vpy;
  rex = rdx + vpx; rey = rdy + vpy;
  pex = pdx + vpx; pey = pdy + vpy;
  # map img points to 3D cylinder in pano radians
  lth = ed * atan( lex );  # pano radians
  lcz = cos( lth );
  lcy = ley * (ed - 1 + lcz); 
  lcx = sin( lth );
  rth = ed * atan( rex );
  rcz = cos( rth );
  rcy = rey * (ed - 1 + rcz);
  rcx = sin( rth );
  pth = ed * atan( pex );
  ptz = cos( pth );
  pty = pey * (ed - 1 + ptz);
  ptx = sin( pth );
  # pano plane on the intersection points (unit normal)
  ppn = normalize(crossp( v3:[rcx, rcy, rcz], v3:[lcx, lcy, lcz]));
  # vertical/radial blend
  refp = vpcyl;
  ty = vsgn * pdy;
  if ty < vy then
    p = 0; 
  else 
    if ty > ry then
      p = 1;
    else 
      p = (ty - vy) / (ry - vy);
    end;
  end;
  refp =  refp * p + v3:[ptx, 0, ptz] * (1 - p);

  # pano plane on pt and the reference point
  ppp = normalize(crossp( v3:[ptx, pty, ptz], refp ));
  # panosphere point on intersection of those planes
  spt = crossp( ppn, ppp );
  # map pano point to cylinder
  cys = vsgn / abs(v2:[spt[0], spt[2]]); # sph to cyl scale
  cyx = spt[0] * cys;  # cylinder x
  cyy = spt[1] * cys;  # cylinder y
  cyz = spt[2] * cys;  # cylinder z
  # map cyl to img
  cis = edp / ( ed - 1 + cyz );	      # cyl to img scale
  tx = pcx + cyx * cis;
  ty = pcy + cyy * cis;

end;

# source point
in(xy:[ tx,ty ])
end
