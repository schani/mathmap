filter radial_mosaic (image in, float angular_size: 0.01-3.141592 (0.0314159),
                      float radial_size: 1-100 (10))
    asz=angular_size;
    rsz=radial_size;
    in(ra-ra%ra:[rsz,asz]+ra:[0,asz/2])
end
