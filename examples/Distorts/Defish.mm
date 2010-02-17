# @title Defish
# @tags distorts

filter defish (image in, float zoom: 0-10 (1),
               float tweak: 0-10 (1))
  in(ra:[atan(r*zoom)*tweak,a])
end