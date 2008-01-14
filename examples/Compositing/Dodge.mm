filter comp_dodge (image in1, image in2)
  p1 = in1(xy);
  p2 = in2(xy);
  p2m = min(p2, rgba:[0.9999, 0.9999, 0.9999, 0.9999]);
  p12 = min(p1 / (-p2m + 1), rgba:[1,1,1,1]);
  rgba:[red(p12), green(p12), blue(p12), min(alpha(p1), alpha(p2))]
end
