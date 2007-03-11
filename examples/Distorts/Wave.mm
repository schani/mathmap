filter wave (image in, float wv: 1-100 (30), float amp: 0-100 (20))
    in(xy+xy:[sin(y/wv+t*2*pi),sin(x/wv+t*2*pi)]*amp)
end
