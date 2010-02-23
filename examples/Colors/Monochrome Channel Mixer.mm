# @title Monochrome Channel Mixer
# @tags colors

filter org.mathmap.bwmixer (image in,
                float red_weight: -4-4 (0.299),
                float green_weight: -4-4 (0.587),
                float blue_weight: -4-4 (0.114))
  p=in(xy);
  g=red(p)*red_weight +
    green(p)*green_weight +
    blue(p)*blue_weight;
  grayaColor(g, alpha(p))
end
