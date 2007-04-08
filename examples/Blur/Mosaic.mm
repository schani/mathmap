unit(square)
filter mosaic (unit(square) image in,
               float size: 0-2 (0.05))
  in(xy-(xy+XY)%size)
end
