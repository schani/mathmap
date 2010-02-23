# @title Hard Light
# @tags compositing

filter org.mathmap.comp_hard_light (image in1, image in2)
  p1 = in1(xy);
  p2 = in2(xy);
  if red(p2) > 0.5 then
    r12 = 1 - (1 - red(p1)) * (1 - (red(p2) - 0.5) * 2)
  else
    r12 = red(p1) * red(p2) * 2
  end;
  if green(p2) > 0.5 then
    g12 = 1 - (1 - green(p1)) * (1 - (green(p2) - 0.5) * 2)
  else
    g12 = green(p1) * green(p2) * 2
  end;
  if blue(p2) > 0.5 then
    b12 = 1 - (1 - blue(p1)) * (1 - (blue(p2) - 0.5) * 2)
  else
    b12 = blue(p1) * blue(p2) * 2
  end;
  rgba:[r12, g12, b12, min(alpha(p1), alpha(p2))]
end
