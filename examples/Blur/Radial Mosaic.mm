filter radial_mosaic (image in, float angular_size: 0.0-3.141592 (0.1),
                      float radial_size: 0-1 (0.05))
    asz=angular_size;
    rsz=radial_size;
    in(ra-ra%ra:[rsz,asz]+ra:[0,asz/2])
end
