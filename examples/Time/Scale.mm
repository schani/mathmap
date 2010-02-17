# @title Scale
# @tags time

filter time_scale (image in, float factor: 0-10 (1))
  in(xy, t * factor)
end
