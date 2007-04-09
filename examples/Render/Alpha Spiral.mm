unit filter alpha_spiral (unit image in, float rotations: 0-20 (5))
    q=sin(r*rotations*pi*2-a+t*2*pi)*0.5+0.5;
    in(xy)*rgba:[1,1,1,0]+rgba:[0,0,0,q]
end
