filter comp_over (image top, image bottom)
  pa = top(xy);
  ma = alpha(pa);
  pb = bottom(xy);
  rgba:[red(pa), green(pa), blue(pa), 1] * ma + pb * (1 - ma)
end
