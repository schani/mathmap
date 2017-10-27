filter anim_rect2ell(image in)
xprime=x/sqrt(1-4*(y/H)*(y/H));
yprime=y/sqrt(1-4*(x/W)*(x/W));
in(xy:[x+t*(xprime-x),y+t*(yprime-y)])
end

filter twist_one(
     image in,
     int Twists: 1-8 (1))

twist=Twists;

angle = 2*pi*t+twist*pi*x/W;

if sign(y)*y>Y*sign(sin(angle))*sin(angle) then
rgba:[0,0,0,0]
else
ny=y/sin(angle);
in(xy:[x,ny]);
end

end 

filter __compos_filter__ (int twist_one_Twists : 1 - 8 (1), image anim_rect2ell_in)
    anim_rect2ell_out = anim_rect2ell(anim_rect2ell_in);
    twist_one_out = twist_one(anim_rect2ell_out, twist_one_Twists);
    twist_one_out(xy)
end
