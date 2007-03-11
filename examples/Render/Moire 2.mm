filter moire2 (float zoom: 1-100 (50))
    grayColor(sin(x*y/zoom+t*2*pi)*0.5+0.5)
end
