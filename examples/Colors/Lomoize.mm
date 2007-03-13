filter lomoize (image in, float brightness: 0-8 (1.0), float contrast: 0-8 (1.6),
                float vignette_radius: 0-1 (0.4), float vignette_hardness: 0-10 (2.0))
    p=in(xy);
    re=clamp(red(p)*2*brightness,0,2);
    gr=clamp(green(p)*2*brightness,0,2);
    bl=clamp(blue(p)*2*brightness,0,2);
    p=rgba:[if re<=1 then (re^contrast)/2 else 1-(2-re)^contrast/2 end,
            if gr<=1 then (gr^contrast)/2 else 1-(2-gr)^contrast/2 end,
            if bl<=1 then (bl^contrast)/2 else 1-(2-bl)^contrast/2 end,
            alpha(p)];
    dist=(toRA(xy*xy:[1/W,1/H]))[0];
    if dist >= vignette_radius then
      f=(1-scale(dist,vignette_radius,1,0,1))^vignette_hardness
    else
      f=1
    end;
    p*rgba:[f,f,f,1]
end