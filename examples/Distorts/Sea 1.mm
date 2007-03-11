filter sea1 (image in, float amp: 0-100 (50), float wv: 1-100 (5))
    in(xy+xy:[amp*(X/H)*sin(t*2*pi+wv*Y*(-y+Y+60)^-1),0])
end
