unit filter sea (unit image in, float amp1: 0-1 (0.03), float amp2: 0-1 (0.01), float wv: 1-100 (5))
    s=sin(t*2*pi+wv*(Y-y+0.1)^-1);
    in(xy+xy:[amp1*s,amp2*s])
end
