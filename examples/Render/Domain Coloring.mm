# Domain coloring.
# Paint the point z=x+iy according to the value of w=f(z), where f is a complex-valued function.
# by Hans Lundmark, http://www.mai.liu.se/~halun/
# see http://www.mai.liu.se/~halun/complex/complex.html

filter domain_coloring (image in, int coloring_scheme: 1-7, gradient coloring)

    i=ri:[0,1];

    #Domain; [mid_x,mid_y]+-[delta_x,delta_y]
    z=ri:( xy:[0,0] + xy:[2,2] * xy/XY );

    #Function
    f=z*z-1 ;

    #Choose coloring scheme
    scheme = coloring_scheme;

    if scheme == 1 then
    # 1. Use active layer superimposed on a rectangle in the w=u+iv plane; [mid_u,mid_v]+-[delta_u,delta_v].
        in((xy:f - xy:[0,0]) / xy:[8,8] * XY)

    else if scheme == 2 then
    # 2. Gradient based on arg(w).
        coloring(pmod(arg(f)/2/pi - 0.0 ,1))

    else if scheme == 3 then
    # 3. Shading based on abs(w); log scale produces one ring for each doubling of abs(w).
        grayColor(pmod(log(abs(f))/log(2),1))

    else if scheme == 4 then
    # 4. Checkerboard (which can be turned into a grid using 'Filters/Edge-Detect/Edge').
        tmpxy=xy:f/  xy:[1,1];
        if (floor(pmod(tmpxy[0],2))+floor(pmod(tmpxy[1],2)))%2 then
            grayColor(1)
        else
            grayColor(0)
        end

    else if scheme == 5 then
    # 5. Checkerboard again, but based on radius and angle.
        tmpra=toRA(xy:(f))/  ra:[1,pi / 6];
        if (floor(pmod(tmpra[0],2))+floor(pmod(tmpra[1],2)))%2 then
            grayColor(1)
        else
            grayColor(0)
        end

    else if scheme == 6 then
    # 6. Color depends on arg(w), intensity on abs(w).
        tmpclr=coloring(pmod(arg(f)/2/pi+  0.0  ,1));
        tmp=((pi/2)-atan(log(abs(f))))/pi;
        c1=max(2*tmp-1,0);
        c2=min(2*tmp,1);
        tmpclr=(-tmpclr+1)*c1+tmpclr*c2;
        rgba:[tmpclr[0],tmpclr[1],tmpclr[2],tmpclr[3]]

    else
    # 7.Yet another idea...
        rgba:[ 0.5+0.5*cos( f[0]*1.6 ), 0.5+0.5*cos( f[1]*1.6 ), 0,1]

    end end end end end end

    # 8. Write your own expressions...
    #
end
