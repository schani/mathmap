# @title Alpha To Gray
# @tags colors

filter org.mathmap.color_alpha_to_gray (image in)
  grayColor(alpha(in(xy)))
end