# @title Radial Eversion
# @tags distorts

# by tomr@aceldama.com
filter org.mathmap.radial_eversion (image in, color outside)
    if (r > 1) then
        outside
    else
        in(ra:[1-r, a])
    end
end
