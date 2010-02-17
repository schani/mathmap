# @title Mix
# @tags compositing

filter comp_mix (image in0, image in1, float blend: 0-1)
  in1(xy) * blend + in0(xy) * (1 - blend)
end
