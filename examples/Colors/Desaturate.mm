# @title Desaturate
# @tags colors

filter org.mathmap.desaturate (image in)
    p=in(xy);
    grayaColor(gray(p),alpha(p))
end
