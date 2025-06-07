#Angled_mirror

filter mirror (image in,
               float angle: 0-360 (0))
    r_angle = deg2rad(angle);
    if (r_angle < pi && a >= r_angle && a < r_angle + pi) ||
       (r_angle > pi && (a >= r_angle || a < r_angle-pi)) then
        new_a = a;
    else
        new_a = 2*r_angle - a;
    end;
    in(ra:[r, new_a])
end 