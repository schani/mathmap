filter curve_bend (image in, float alpha: 0-6.28318530,
                   curve lower, curve upper)
    dir = xy:[cos(alpha),sin(alpha)];
    ndir = xy:[-dir[1],dir[0]];
    p = xy / m2x2:[dir[0],-ndir[0],
                   dir[1],-ndir[1]];
    pt = dir * p[0];
    vec = xy - pt;
    dist = -p[1];
    pos = 0.5 + p[0] / 2;
    lo = 1 / (lower(pos) * 4 - 2);
    up = 1 / (upper(pos) * 4 - 2);
    f = lo + ((dist + 1) / 2) * (up - lo);
    in(pt + ndir * f)
end
