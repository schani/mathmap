unit filter sea2 (unit image in, float amp: 0-1 (0.01), float wv: 1-100 (7))
    in(xy+xy:[0,amp*sin(t*2*pi+wv*(Y-y+0.1)^-1)])
end
