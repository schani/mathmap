# @title Tint
# @tags colors

filter org.mathmap.color_tint (image in, float hue: 0-1, float saturation: 0-1)
  p = in(xy);
  g = 1 - gray(p);
  if g < 0.5 then
    sat = g * 2;
    val = 1
  else
    sat = 1;
    val = 1 - (g - 0.5) * 2
  end;
  o = toRGBA(hsva:[hue, sat, val, alpha(p)]);
  lerp(saturation, p, o)
end 