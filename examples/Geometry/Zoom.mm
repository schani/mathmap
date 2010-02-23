# @title Zoom
# @tags geometry

filter org.mathmap.geom_zoom (image in, float factor: 0-10 (1))
  in(xy * factor)
end
