# by Hans Lundmark, http://www.mai.liu.se/~halun/
# see http://www.mai.liu.se/~halun/complex/complex.html

unit filter grid_based_domain_coloring ()
    z = x + I*y;
    # This sets the corners of the window
    # to 1.2 + 1.2i, 1.2 - 1.2i, -1.2 + 1.2i, -1.2 - 1.2i
    # (provided your image is square):
    z = 1.2*z;
    # Change this to the function you want, e.g.
    # w=sin(1/z);
    w = 1/z;
    # This determins the spacing of the grid:
    spacing=1;
    w = w/spacing;
    rgrid = pmod(w[0],1);
    igrid = pmod(w[1],1);
    # This creates a black and white "checkerboard".
    # Other ideas:
    # gradient((rgrid + igrid)/2)
    # or
    # grayColor (rgrid*igrid)
    grayColor((rgrid < 1/2) != (igrid < 1/2))
end
