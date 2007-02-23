filter wbawt (float granularity: 0-50, float z1: 0-10, float z2: 0-10, float w2: 0-1, float f2: 1-10, float threshold: -1-1)
    g = granularity;
    nxy = xy/R*g;
    thr = threshold;
    n=noise([nxy[0]/2,nxy[1],z1])+noise([nxy[0]/2*f2,nxy[1]*f2,z2])*w2;
    grayColor(if n >= thr then 1 else 0 end)
end
