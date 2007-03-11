filter bilin_int (image in,
                  float a0: -1-1 (0), float a1: -1-1 (-1), float a2: -1-1 (1), float a3: -1-1 (0),
                  float b0: -1-1 (0), float b1: -1-1 (1),  float b2: -1-1 (1), float b3: -1-1 (0))
    xn=x/X;
    yn=y/Y;
    u=xn*yn*a3+xn*a2+yn*a1+a0;
    v=xn*yn*b3+xn*b2+yn*b1+b0;
    in(xy:[u*X,v*Y])
end
