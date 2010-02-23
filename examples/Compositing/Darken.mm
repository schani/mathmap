# @title Darken
# @tags compositing

filter org.mathmap.comp_darken (image in1, image in2)
  p1 = in1(xy);
  p2 = in2(xy);
  p12 = min(p1, p2);
  rgba:[red(p12), green(p12), blue(p12), min(alpha(p1), alpha(p2))]
end
