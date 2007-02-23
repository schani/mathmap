filter sphere (image in, float mag: 0.1-10, color background)
    p=r/X;
    if inintv(p,0,1) then
        in(ra:[X/pi/2*asin(p)*mag,a])
    else
        background
    end
end
