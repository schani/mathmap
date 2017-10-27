#Time Rotate (c)Nevit Dilmen 2009
filter rotate_time (image in)
  in(ra + ra:[0, 2*pi*t])
end

#Time log zoom by Edgar 
filter time_zoom (image in)
zoom = sin(t*pi/2);
in(xy * exp(scale(zoom, 0, 1, 4, 0)))
end


filter __composer_filter__ (image rotate_time_in)
    rotate_time_out = rotate_time(rotate_time_in);
    time_zoom_out = time_zoom(rotate_time_out);
    time_zoom_out(xy)
end
