# @title Mugl
# @tags map

# by Herbert Poetzl
pixel
filter org.mathmap.mugl (pixel image in, int edges: 3-10 (5),
             float grid_size: 10-300 (100), float polygon_size: 1.2-5 (2.0),
             color edge_color, float edge_width: 0-50 (5),
             color background)
    sl=grid_size;
    nx=floor(x/sl-0.5); ny=floor(y/sl-0.5);
    muglwinkl=pi/edges;
    heuslfaktor=sl/polygon_size;
    i=0; found=0;
    while !found && i < 4 do
        ix=nx+i%2;
        iy=ny+floor(i/2);
        xc=(ix+0.5)*sl;
        yc=(iy+0.5)*sl;
        alpha=(noise([ix*0.3,iy*0.3,0])+1)*pi+t*pi*2;
        kurde=toRA(xy:[x-xc,y-yc])+ra:[0,pmod(alpha,pi*2)];
        radii=heuslfaktor/cos(abs(kurde[1]%(muglwinkl*2)-muglwinkl));
        heuslkurde=toRA(xy:[x-xc,y-yc])+ra:[0,alpha];
        if heuslkurde[0]<radii then
            found = 1
        else
            i=i+1
        end
    end;
    if i >= 4 then
        background
    else
        if heuslkurde[0] < radii-edge_width then
            in(toXY(heuslkurde)+xy:[xc,yc])
        else
            edge_color
        end
    end
end
