# by Alexander Heide <heide@ra.physik.uni-halle.de>
# see http://www.physik.uni-halle.de/~heide/crystal/crystal.html
filter stereographic (image in, float x_center: 0-2048, float y_center: 0-2048, float distance: 0-1024,
                      float zoom: -1-1, bool trans_or_back, color background)
    xy0=xy:[x_center-X, -(y_center-Y)];
    D=distance;
    s=zoom;
    pxy=(xy*10^-s)/min(X,Y);
    p=(dotp(pxy,pxy))^(1/2);
    back=trans_or_back;
    xy1=pxy*(2*back-1)/p*D*tan(4*atan(p))+xy0;
    out=0;
    if back then
        if p>tan(45/2) then out=1 end
    else
        if p>1 || p<tan(45/2) then out=1 end
    end;
    if out || abs(xy1[0])>=X-1 || abs(xy1[1])>=Y-1
    then background
    else in(xy1)
    end
end
