filter sea3 (image in, float amp1: 0-100, float amp2: 0-100, float wv: 1-100)
    s=sin(t*2*pi+wv*Y*(-y+Y+60)^-1);
    in(xy+xy:[amp1*s,amp2*s])
end
