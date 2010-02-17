# @title Threshold
# @tags colors

filter threshold (image in, float threshold: 0-1 (0.5))
  grayColor(if gray(in(xy)) <= threshold then 0 else 1 end)
end
