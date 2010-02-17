# @title Alpha To Gray
# @tags colors

filter color_alpha_to_gray (image in)
  grayColor(alpha(in(xy)))
end