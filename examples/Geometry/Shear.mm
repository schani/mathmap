# @title Shear
# @tags geometry

filter org.mathmap.geom_shear (image in, float ax: -10-10 (0), float ay: -10-10 (0))
  in(xy - xy:[y * ax, x * ay])
end