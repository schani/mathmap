filter random_edge_detect (image in, float distance: 0-50)
    d=distance;
    in(xy)-in(xy-xy:[rand(0,d),rand(0,d)])
end
