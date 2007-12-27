filter sphere (image in, float mag: 0-10 (5), color background)
    if inintv(r,0,1) then
        in(ra:[asin(r)*mag/pi/2,a])
    else
        background
    end
end
