filter wave (image in, float wv: 1-100, float amp: 0-100)
    in(xy+xy:[sin(y/wv+t*2*pi),sin(x/wv+t*2*pi)]*amp)
end
