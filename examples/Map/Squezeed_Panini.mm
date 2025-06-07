filter eqr_sqeezedpanini (
  image in,
  float eyed: 0-2 (1),
  float pan: -180-180 (0),
  float zoom: 0.25-2 (1),
  float hshf: -1-1 (0),
  float vshf: -1-1 (0),
  float sqz: 0-1 (0.5),
  float sqw: 0-1.5 (1),
  float sqa: -45-45 (0)
)

## 360x180 equirectangular to squeezed panini
# (c) copyright 2009 Thomas K Sharpless
# free license is granted for noncommercial use
##
# parameters
Sppr = W / (2*pi);      # source pixels/radian
d = eyed + 1;
wfov = pi * 150 / 180;  # radians
Drpp = 2*d*tan(wfov/(2*d)) / W;
tau = sqa * pi / 180;
qtw = sqw * pi;
qwa = pi;
qwr = 0.5 * (qwa - qtw);
qwl = -qwr;
qfl = cos( tau + qwl );
qfr = cos( tau - qwr );
qsl = 1;
qsr = 1;
if abs(qfl) > 0.00000000001 then
  qsl = qfl * (eyed + 1) / (cos(qwl) + eyed)
end;
if abs(qfr) > 0.00000000001 then
  qsr = qfr * (eyed + 1) / (cos(qwr) + eyed)
end;
xshf = X * hshf;
yshf = Y * vshf;

# destination coordinates in radians
xr = (x - xshf) * Drpp / zoom;
yr = (y - yshf) * Drpp / zoom;

# project dest to cylinder
azi = d * atan( xr, d );
fp = (eyed + cos(azi))/d;       # panini factor
fs = cos(azi - tau);            # squeezed factor

# taper the squeeze factor
if azi > qwr then
  tt = max(0,min(1,(azi-qwr) / qtw ));
  fs = (tt) * qsr * fp + (1-tt) * fs;
else if azi < qwl then
  tt = max(0,min(1,(qwl-azi) / qtw ));
  fs = (tt) * qsl * fp + (1-tt) * fs;
end end;

# project linear mix of cyl. hgts to sphere
alt = atan( yr * (fp + sqz * (fs - fp)) );

# source coordinates in pixels
sx = Sppr*azi;
sy = Sppr*alt;

# pan and wrap
sx = sx + W*pan/360;
if sx > X then sx = sx - W end;
if sx < -X then sx = sx + W end;

#interpolate
in(xy:[sx, sy])

end 