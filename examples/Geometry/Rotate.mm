# @title Rotate
# @tags geometry

filter org.mathmap.geom_rotate (image in, float angle: 0-6.283)
  in(ra + ra:[0, angle])
end