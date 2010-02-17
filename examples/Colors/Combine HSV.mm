# @title Combine HSV
# @tags colors

filter color_combine_hsv (image in_hue, image in_saturation, image in_value)
  toRGBA(hsva:[gray(in_hue(xy)), gray(in_saturation(xy)), gray(in_value(xy)), 1])
end