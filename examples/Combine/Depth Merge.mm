filter depth_merge (image image1, image image2, image depth1, image depth2, 
                    float overlap: 0.001-2 (1.0), float offset: -1-1 (0),
                    float scale1: -1-1 (0.5), float scale2: -1-1 (-0.5))
    d1 = gray(depth1(xy));
    d2 = gray(depth2(xy));
    frac = clamp(((d2 * scale2 - (d1 * scale1 + offset)) / overlap + 1) / 2, 0, 1);
    c1 = image1(xy);
    c2 = image2(xy);
    clamp(lerp(frac,c2,c1),rgba:[0,0,0,0],rgba:[1,1,1,1])
end
