# @title In
# @tags compositing

filter comp_in (image in, image stencil)
  in(xy) * alpha(stencil(xy))
end
