# by tomr@aceldama.com
unit filter radial_eversion (unit image in, color outside)
    if (r > 1) then
        outside
    else
        in(ra:[1-r, a])
    end
end
