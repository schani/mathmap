filter brightness_contrast (image in, float brightness: 0-8, float contrast: 0-8)
    p=in(xy);
    re=clamp(red(p)*2*brightness,0,2);
    gr=clamp(green(p)*2*brightness,0,2);
    bl=clamp(blue(p)*2*brightness,0,2);
    rgba:[if re<=1 then (re^contrast)/2 else 1-(2-re)^contrast/2 end,
          if gr<=1 then (gr^contrast)/2 else 1-(2-gr)^contrast/2 end,
          if bl<=1 then (bl^contrast)/2 else 1-(2-bl)^contrast/2 end,
          alpha(p)]
end

filter vignette (image in, float radius: 0-1, float hardness: 0-10)
    dist=(toRA(xy/sqrt(2)))[0];
    if dist >= radius then
      f=(1-scale(dist,radius,1,0,1))^hardness
    else
      f=1
    end;
    in(xy)*rgba:[f,f,f,1]
end

stretched
filter lomoize (stretched image in,
                float brightness: 0-8 (1.0), float contrast: 0-8 (1.6),
                float vignette_radius: 0-1 (0.4), float vignette_hardness: 0-10 (2.0))
  vignette(brightness_contrast(in, brightness, contrast),
           vignette_radius, vignette_hardness,
           xy)
end