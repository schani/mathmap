filter color_invert (image in)
  p = in(xy);
  rgba:[1 - red(p), 1 - green(p), 1 - blue(p), alpha(p)]
end
