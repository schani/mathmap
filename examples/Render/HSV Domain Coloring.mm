# by Hans Lundmark, http://www.mai.liu.se/~halun/
# see http://www.mai.liu.se/~halun/complex/complex.html

unit filter hsv_domain_coloring ()
    z = x + I*y;
    # This sets the corners of the window
    # to 1.2 + 1.2i, 1.2 - 1.2i, -1.2 + 1.2i, -1.2 - 1.2i
    # (provided your image is square):
    z = 1.2*z;
    # Change this to the function you want, e.g.
    # w=sin(1/z);
    w = 1/z;
    hue = arg(w)/2/pi + 1/2;
    if !inintv(hue,0,1) then hue = 0 end;
    sat = atan(abs(2*w))/(pi/2);
    val = 1 - atan(abs(w/8))/(pi/2);
    toRGBA(hsva:[hue,sat,val,1])
end
