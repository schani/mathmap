# by Alexander Heide <heide@ra.physik.uni-halle.de>
# see http://www.physik.uni-halle.de/~heide/crystal/crystal.html
filter rotation (image in, float x_center: -1024-1024, float y_center: -1024-1024,
                 float psi: 0-3.141592, float theta: -3.141592-3.141592, float radius: 1-2048,
                 float x_shift: -2048-2048, float y_shift: -2048-2048, float zoom: -1-1)
    x0=x_center-X;
    y0=-(y_center-Y);

    rad=radius;

    xs=-x_shift;
    ys=-y_shift;
    s=-zoom;

    xy1=xy*10^s+xy:[xs,ys];

    pxy=xy1-xy:[x0,y0];

    pxy=pxy/rad;
    psq=pxy[0]^2+pxy[1]^2;
    xi=2*pxy[0]/(1+psq);
    eta=2*pxy[1]/(1+psq);
    zeta=(1-psq)/(1+psq);

    xi1=cos(psi)*xi+sin(psi)*eta;
    eta1=-sin(psi)*xi+cos(psi)*eta;
    zeta1=zeta;

    xi=xi1;
    eta=cos(theta)*eta1+sin(theta)*zeta1;
    zeta=-sin(theta)*eta1+cos(theta)*zeta1;

    xi1=cos(psi)*xi-sin(psi)*eta;
    eta1=sin(psi)*xi+cos(psi)*eta;
    zeta1=zeta;

    px=xi1/(1+zeta1)*rad+x0;
    py=eta1/(1+zeta1)*rad+y0;

    in(xy:[px,py])
end
