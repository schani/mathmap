# @title Invert
# @tags time

filter time_invert (image in)
  in(xy, 1-t)
end
