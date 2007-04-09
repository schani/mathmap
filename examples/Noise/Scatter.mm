unit filter scatter (unit image in, float distance: 0-1 (0.02))
    d=distance;
    in(xy+xy:[rand(-d,d),rand(-d,d)]*t)
end
