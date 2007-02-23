filter circular_slice (image in, float ang: 0-1)
    in(ra:[r,a+(r/200+t*ang)%ang-ang/2])
end
