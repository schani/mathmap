# by tomr@aceldama.com
filter radial_eversion (image in, color outside)
    if (r > X) then
        outside
    else
        in(ra:[X-r, a])
    end
end
