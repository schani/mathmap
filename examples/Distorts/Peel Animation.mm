# tomr@aceldama.com
filter peel (image in, color outside)
    if (r > X) then
        outside
    else
        rad = (X*t);
        if (r > rad) then
            in(ra:[rad*(1-(r-rad)/(X-rad)), a]);
        else
            in(xy);
        end
    end
end
