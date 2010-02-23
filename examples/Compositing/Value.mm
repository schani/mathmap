# @title Value
# @tags compositing

filter org.mathmap.comp_value (image in1, image in2)
  p1 = toHSVA(in1(xy));
  p2 = toHSVA(in2(xy));
  toRGBA(hsva:[p1[0], p1[1], p2[2], min(p1[3], p2[3])])
end
