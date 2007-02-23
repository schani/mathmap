filter mandelbrot (gradient coloring)
    p=ri:(xy/xy:[X,X]*1.5-xy:[0.5,0]);
    c=ri:[0,0];
    iter=0;
    while abs(c)<2 && iter<31
    do
        c=c*c+p;
        iter=iter+1
    end;
    coloring(iter/32)
end
