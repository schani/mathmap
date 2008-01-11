filter comp_out (image in, image stencil)
  in(xy) * (1 - alpha(stencil(xy)))
end
