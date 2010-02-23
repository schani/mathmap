# @title Fisheye
# @tags distorts

filter org.mathmap.fisheye (image in, float amt: -2-1 (0))
    in(ra:[r^(2-amt)/R^(1-amt),a])
end
