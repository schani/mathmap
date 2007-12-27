# by tomr@aceldama.com
filter radial_eversion (image in, color outside)
    if (r > 1) then
        outside
    else
        in(ra:[1-r, a])
    end
end
