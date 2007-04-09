unit(stretched)
filter bilin_int (unit(stretched) image in,
                  float a0: -1-1 (0), float a1: -1-1 (0), float a2: -1-1 (1), float a3: -1-1 (0),
                  float b0: -1-1 (0), float b1: -1-1 (1),  float b2: -1-1 (0), float b3: -1-1 (0))
    u=x*y*a3+x*a2+y*a1+a0;
    v=x*y*b3+x*b2+y*b1+b0;
    in(xy:[u,v])
end
