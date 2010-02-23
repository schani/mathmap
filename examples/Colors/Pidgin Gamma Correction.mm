# @title Pidgin Gamma Correction
# @tags colors

filter org.mathmap.pidgin_gamma_correction (image in, curve gamma)
"This is not actually a gamma correction.  Gamma correction applies the gamma curve to each component (red, green, blue) independently.  This effect applies the gamma curve to the luminosity only."
    p=in(xy);
    g=gray(p);
    p*rgba:[gamma(g)/g,gamma(g)/g,gamma(g)/g,alpha(p)]
end
