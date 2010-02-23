# @title Grain Extract
# @tags compositing

filter org.mathmap.comp_grain_extract (image in1, image in2)
  p1 = in1(xy);
  p2 = in2(xy);
  p12 = clamp(p1 - p2 + 0.5, grayColor(0), grayColor(1));
  rgba:[red(p12), green(p12), blue(p12), min(alpha(p1), alpha(p2))]
end
