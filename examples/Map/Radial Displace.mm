# tomr@aceldama.com
filter radial_displace (image in, image radial, float max_r: 0-32, image angle, float max_a: 0-6.2831853)
    in(ra:[r+t*max_r*((2*gray(radial(xy)))-1),a+t*max_a*((2*gray(angle(xy)))-1)])
end
