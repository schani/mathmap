#Time Zoom (c)Nevit Dilmen
filter time_zoom (image in)
zoom = sin(t*pi/2);
  in(xy * scale(zoom, 0, 1, 50, 1))
end
