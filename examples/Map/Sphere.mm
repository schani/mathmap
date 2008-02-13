# by Herbert Poetzl
filter map_sphere (image in,
                   float alpha: 0-6.2831853 (3.4), float beta: 0-6.2831853 (4.2),
                   float gamma: 0-6.2831853 (5.2), color background)
    rd=0.9*min(X,Y);
    if r>rd then
        background
    else
        sa=sin(alpha);
        sb=sin(beta);
        ca=cos(alpha);
        cb=cos(beta);
        theta=a;
        phi=acos(r/rd);
        x0=cos(theta)*cos(phi);
        y0=sin(theta)*cos(phi);
        z0=sin(phi);
        x1=ca*x0+sa*y0;
        z1=-sa*-sb*x0+ca*-sb*y0+cb*z0;
        if z1 >= 0 || 1 then
            y1=cb*-sa*x0+cb*ca*y0+sb*z0
        else
            z1=z1-2*cb*z0;
            y1=cb*-sa*x0+cb*ca*y0-sb*z0
        end;
        theta1=atan(-x1/y1)+(if y1>0 then pi/2 else 3*pi/2 end);
        phi1=asin(z1);
        in(xy:[((theta1*2+gamma)%(2*pi)-pi)/pi*X,-phi1/(pi/2)*Y])
    end
end
