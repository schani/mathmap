# @title In
# @tags compositing

filter org.mathmap.comp_in (image in, image stencil)
  in(xy) * alpha(stencil(xy))
end
