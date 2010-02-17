# @title Sphere with Reflection
# @tags map

# by Herbert Poetzl
filter sphere_with_reflection (image in, float alpha: 0-6.2831853 (3.4),
                               float beta: 0-6.2831853 (4.2), float gamma: 0-6.2831853 (5.2),
                               float lx: -1-1 (0.3), float ly: -1-1 (-0.3), float lz: -1-1 (-0.7),
                               color background)
    rd=0.9*min(X,Y);
    if r>rd then
        background
    else
        phil=pi/4;
        lv=normalize([lx,ly,lz]);
        sa=sin(alpha);
        sb=sin(beta);
        ca=cos(alpha);
        cb=cos(beta);
        theta=a;
        phi=acos(r/rd);
        m0=cos(phi);
        x0=cos(theta)*m0;
        y0=sin(theta)*m0;
        z0=sin(phi);
        x1=ca*x0+sa*y0;
        m1=ca*y0-sa*x0;
        z1=cb*z0-sb*m1;
        y1=cb*m1+sb*z0;
        diffq=dotp(lv,[x1,y1,z1]);
        theta1=atan(-x1/y1)+(if y1>0 then pi/2 else 3*pi/2 end);
        phi1=asin(z1);
        p=in(xy:[((theta1+gamma)%(2*pi)-pi)/pi*X,-phi1/(pi/2)*Y]);
        p*(1-diffq)+rgba:[1,1,1,1]*diffq
    end
end
