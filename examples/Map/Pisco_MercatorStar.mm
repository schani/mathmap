unit filter MercatorStars (
unit image in,
int Spin: -180-180 (90),
float Latitude: -180-180 (90),
int Longitude: -180-180 (90),
int Points: 2-10 (3),
int Zoom: 1-100 (50))

z = ri:[x,y];
ZOOM = (100-Zoom)/Zoom*10;
z = z*ZOOM;
z = tan(z^(Points))+(Longitude/100);
LATITUDE = Latitude;
SPIN = -(pi/180) * Spin;
z=z/LATITUDE*exp(-I*SPIN);
zxy = xy:[z[0],z[1]];

in(zxy)

end