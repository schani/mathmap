# @title Translate
# @tags geometry

filter geom_translate (image in, float dx: -10-10 (0), float dy: -10-10 (0))
  in(xy - xy:[dx, dy])
end