filter anim_rect2ell(image in)
xprime=x/sqrt(1-4*(y/H)*(y/H));
yprime=y/sqrt(1-4*(x/W)*(x/W));
in(xy:[x+t*(xprime-x),y+t*(yprime-y)])
end