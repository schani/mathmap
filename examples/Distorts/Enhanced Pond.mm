filter enhanced_pond (image in, float max_freq: 1-10000, float amp: 0-100, float r_off: 1-1000)
    in(ra+ra:[sin(max_freq/(r+r_off)+t*2*pi)*amp,0])
end
