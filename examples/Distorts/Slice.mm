filter slice (image in, float size: 0-1 (0.03), float offset:0-1 (0.05))
    q=t*2*pi;
    in(xy+xy:[offset*sign(cos(y/size+q)),offset*sign(cos(x/size+q))])
end
