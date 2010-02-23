# @title Wave
# @tags distorts

filter org.mathmap.wave (image in,
             float wv: 0-1 (0.05),
             float amp: 0-1 (0.03))
    in(xy+xy:[sin(y/wv+t*2*pi),
              sin(x/wv+t*2*pi)]*amp)
end
