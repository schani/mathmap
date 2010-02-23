# @title Hue
# @tags compositing

filter org.mathmap.comp_hue (image in1, image in2)
  p1 = toHSVA(in1(xy));
  p2 = toHSVA(in2(xy));
  toRGBA(hsva:[p2[0], p1[1], p1[2], min(p1[3], p2[3])])
end
