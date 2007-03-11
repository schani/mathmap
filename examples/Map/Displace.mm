filter displace (image in,
                 image x_image, float x_disp: 0-1000 (100),
                 image y_image, float y_disp: 0-1000 (100))
    in(xy+xy:[(gray(x_image(xy))-0.5)*x_disp*2,(gray(y_image(xy))-0.5)*y_disp*2])
end
