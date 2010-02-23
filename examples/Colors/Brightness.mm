# @title Brightness
# @tags colors

filter org.mathmap.color_brightness (image in, float brightness: -1-1 (0))
  p = in(xy);
  if brightness < 0 then
    v = p * (brightness + 1)
  else
    v = p + (rgba:[1,1,1,1] - p) * brightness
  end;
  rgba:[red(v), green(v), blue(v), alpha(p)]
end