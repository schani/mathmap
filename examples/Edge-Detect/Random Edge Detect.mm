filter random_edge_detect (image in, float distance: 0-1 (0.01))
    d=distance;
    in(xy)-in(xy-xy:[rand(0,d),rand(0,d)])
end
