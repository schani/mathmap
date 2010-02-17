# @title Gamma Correction
# @tags colors

filter gamma_correction (image in, curve gamma)
    p=in(xy);
    rgba:[gamma(red(p)),gamma(green(p)),gamma(blue(p)),alpha(p)]
end
