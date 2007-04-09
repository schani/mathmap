unit filter enhanced_pond (unit image in, float max_freq: 1-10 (4),
                           float amp: 0-1 (0.02), float r_off: 0-1 (0.1))
    in(ra+ra:[sin(max_freq/(r+r_off)+t*2*pi)*amp,0])
end
