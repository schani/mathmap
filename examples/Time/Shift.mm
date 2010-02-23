# @title Shift
# @tags time

filter org.mathmap.time_shift (image in, float offset: -1-1 (0))
  in(xy, t - offset)
end
