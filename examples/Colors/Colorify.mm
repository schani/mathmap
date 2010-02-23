# @title Colorify
# @tags colors

filter org.mathmap.colorify (image in, gradient colors)
  p = in(xy);
  c = colors((gray(p)+t)%1);
  rgba:[c[0], c[1], c[2], c[3] * p[3]]  
end
