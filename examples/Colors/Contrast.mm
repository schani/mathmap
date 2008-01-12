filter color_contrast (image in, float contrast: -1-1)
  p = in(xy);
  slant = tan((contrast + 0.9999) * (pi / 4));
  v = (p - 0.5) * slant + 0.5;
  rgba:[red(v), green(v), blue(v), alpha(p)]
end