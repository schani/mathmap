filter pidgin_gamma_correction (image in, curve gamma)
    p=in(xy);
    g=gray(p);
    p*rgba:[gamma(g)/g,gamma(g)/g,gamma(g)/g,alpha(p)]
end
