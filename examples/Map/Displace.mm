filter displace (image in,
                 image x_image, float x_disp: 0-1 (0.1),
                 image y_image, float y_disp: 0-1 (0.1))
"This is pretty much a traditional displace filter.  It takes an input image and two displace images (one for each direction), which determine by their brightness how much to displace each pixel."
    in(xy+xy:[(gray(x_image(xy))-0.5)*x_disp*2,(gray(y_image(xy))-0.5)*y_disp*2])
end
