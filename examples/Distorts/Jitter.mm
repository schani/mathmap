# @title Jitter
# @tags distorts

filter org.mathmap.jitter (image in, int num_divisions: 1-100 (10))
    angle=pi*2/num_divisions;
    in(ra:[r,a+(a+t*angle)%angle-angle/2])
end
