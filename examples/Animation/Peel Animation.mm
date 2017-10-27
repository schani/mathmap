# tomr@aceldama.com
filter peel (image in, color outside)
    if (r > 1) then
        outside
    else
        if (r > t) then
            in(ra:[t*(1-(r-t)/(1-t)), a]);
        else
            in(xy);
        end
    end
end
