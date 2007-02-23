filter radial_mosaic (image in, float angular_size: 0.01-3.141592,
                      float radial_size: 1-100)
    asz=angular_size;
    rsz=radial_size;
    in(ra-ra%ra:[rsz,asz]+ra:[0,asz/2])
end
