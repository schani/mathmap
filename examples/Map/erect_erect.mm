filter erect_erect(image in,
                     float pitch: -180-180 (0),
                     float roll: -90-90 (0),
                     float yaw: -180-180 (0) )

  ## Rotates the sphere represented by equirectangular projection
  ## roll, pitch and yaw angles (in degrees) that the sphere is rotated.
  ## z axis though the poles.
  ## PJ Gawthrop, July 2008
  
  ## Rads/pixel
  rads = pi/H;
  
  ## Degrees to radians
  x_rad = roll*pi/180;
  y_rad = pitch*pi/180;
  z_rad = yaw*pi/180;

  ## Latitude and longditude
  lon = x*rads;
  lat = y*rads;

  ## Cartesian coordinates
  x1 = cos(lat)*cos(lon);
  y1 = cos(lat)*sin(lon);
  z1 = sin(lat);

  ## Rotate about y axis
  s = sin(y_rad);
  c = cos(y_rad);
  x2 =  c*x1 + s*z1;
  y2 =  y1;
  z2 = -s*x1 + c*z1;

  ## Rotate about x axis
  s = sin(x_rad);
  c = cos(x_rad);
  x3 = x2;
  y3 =  c*y2 + s*z2;
  z3 = -s*y2 + c*z2;


  ## Rotate about z axis
  s = sin(z_rad);
  c = cos(z_rad);
  x4 =  c*x3 + s*y3;
  y4 = -s*x3 + c*y3;
  z4 = z3;

  ## Transform back again
  lat = asin(z4);
  lon = atan(y4,x4);


  in(xy:[lon/rads, lat/rads])
end
