# we should have an "include" facility for this

filter soft_light_multiply (image in1, image in2)
  p1 = in1(xy);
  p2 = in2(xy);
  p12 = p1 * p2;
  rgba:[red(p12), green(p12), blue(p12), min(alpha(p1), alpha(p2))]
end

filter soft_light_screen (image in1, image in2)
  p1 = in1(xy);
  p2 = in2(xy);
  p12 = -(-p1 + 1) * (-p2 + 1) + 1;
  rgba:[red(p12), green(p12), blue(p12), min(alpha(p1), alpha(p2))]
end

filter comp_soft_light (image in1, image in2)
  mult = soft_light_multiply(in1, in2, xy);
  screen = soft_light_screen(in1, in2, xy);
  p1 = in1(xy);
  p2 = in2(xy);
  p12 = (-p1 + 1) * mult + p1 * screen;
  rgba:[red(p12), green(p12), blue(p12), min(alpha(p1), alpha(p2))]
end
