# @title Divide
# @tags compositing

filter comp_divide (image in1, image in2)
  p1 = in1(xy);
  p2 = in2(xy);
  p2m = max(in2(xy), rgba:[0.0001, 0.0001, 0.0001, 0.0001]);
  p12 = p1 / p2m;
  rgba:[red(p12), green(p12), blue(p12), min(alpha(p1), alpha(p2))]
end
