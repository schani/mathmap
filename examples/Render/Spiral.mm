# @title Spiral
# @tags render

filter spiral (float rotations: 0-20 (5))
    q=sin(r*rotations*pi*2-a+t*2*pi)*0.5+0.5;
    grayColor(q)
end
