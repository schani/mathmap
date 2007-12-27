filter circular_slice (image in, float ang: 0-1 (0.15))
    in(ra:[r,a+(r+t*ang)%ang-ang/2])
end
