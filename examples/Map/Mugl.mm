# by Herbert Poetzl
filter mugl (image in, int edges: 3-10, float size: 1.2-5)
    sl=30;
    nx=floor(x/sl-0.5); ny=floor(y/sl-0.5);
    alpha=[0,0,0,0];
    radii=[0,0,0,0];
    xc=[0,0,0,0];
    yc=[0,0,0,0];
    muglwinkl=pi/edges;
    heuslfaktor=sl/size;
    i=0; while i < 4 do
        ix=nx+i%2;
        iy=ny+floor(i/2);
        xc[i]=(ix+0.5)*sl;
        yc[i]=(iy+0.5)*sl;
        alpha[i]=(noise([ix*0.3,iy*0.3,0])+1)*pi+t*pi*2;
        kurde=toRA(xy:[x-xc[i],y-yc[i]])+ra:[0,pmod(alpha[i],pi*2)];
        radii[i]=heuslfaktor/cos(abs(kurde[1]%(muglwinkl*2)-muglwinkl));
        i = i + 1
    end;
    i=0;found=0;
    while !found && i<4 do
        heuslkurde=toRA(xy:[x-xc[i],y-yc[i]])+ra:[0,alpha[i]];
        if heuslkurde[0]<radii[i] then
            found = 1
        else
            i=i+1
        end
    end;
    if i >= 4 then
        rgba:[0,0,1,1]
    else
        if heuslkurde[0] < radii[i]-1.5 then
            in(toXY(heuslkurde)+xy:[xc[i],yc[i]])
        else
            rgba:[0,0,0,1]
        end
    end
end
