filter comp_addition_with_opacity (image in1, image in2, float opacity: 0-1 (1))
  p1 = in1(xy);
  p2 = in2(xy);
  p12 = p1 + p2;
  p12o = lerp(opacity, p1, p12);
  rgba:[red(p12o), green(p12o), blue(p12o), min(alpha(p1), alpha(p2))]
end
