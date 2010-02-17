# @title Inverse Lambert Azimuthal Projection
# @tags distorts

# Thanks to Carlos A. Furuti
filter ilap (image in)
    c=2*asin(r/Y);
    phi =
        if r == 0 then
            0
        else
            asin(y*sin(c)/r)
        end;
    in(xy:[atan(x*sin(c),r*cos(c))*X/pi,phi*Y/(pi/2)])
end
