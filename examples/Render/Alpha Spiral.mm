filter alpha_spiral (image in, float rotations: 0-20)
    q=sin((r/R*rotations-a*0.1)*10+t*2*pi)*0.5+0.5;
    in(xy)*rgba:[1,1,1,0]+rgba:[0,0,0,q]
end
