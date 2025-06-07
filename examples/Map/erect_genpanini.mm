filter erect_genpanini( image in, 
float Yaw: -90-90 (0),
float Hcmp: 0-2 (1),
float Vcmp: 0-2 (0),
float Vfac: 0-2(1),
float Hscl: 0.5-2 (1),
float Vscl: 0.5-2 (1),
float Vshf: -1-1 (0)
)
# equirectangular to generalized Pannini
# Tom Sharpless Jan 2009
# input is a 360 x 180 equirectangular image
#  centered on the desired vanishing point
#  (however yaw can be fine tuned here)
# Fov is angular width of result
# Hcmp and Vcmp adjust compression between
#   linear / stereographic/ orthographic
# Vfac scales the x-dependent Pannini y factor
# Vscl scales the height linearly
# Vshf is a linear framing shifts
ppr = H/(pi);
rpd = pi/180;

xsh = Yaw * W / 360;

dh = if Hcmp < 1.96 then
  ppr * (1 + tan(45 * Hcmp * rpd))
else
  ppr * 50
end;
dv = if Vcmp < 1.96 then
  ppr * (1 + tan(45 * Vcmp * rpd));
else
  ppr * 50;
end;

phi = atan(x / dh);
ysc = (Vfac * cos(phi)*cos(phi) + 1 - Vfac) / Vscl;
xsc = 1 / Hscl;
ysh = 0.5 * H * Vshf;

in(xy:[xsc*dh*phi + xsh, ysc*dv*atan((y + ysh)/dv)])

end 
