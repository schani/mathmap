# @title Moire 2
# @tags render

filter moire2 (float zoom: 1-10000 (100))
    grayColor(sin(x*y*zoom+t*2*pi)*0.5+0.5)
end
