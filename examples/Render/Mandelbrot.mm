unit filter mandelbrot (float zoom: 0-2 (1.5), float xoff: -1-2 (0.5), float yoff: -1-1 (0),
                        int num_iterations: 2-256 (32), gradient coloring)
    p=ri:(xy*zoom-xy:[xoff,yoff]);
    c=ri:[0,0];
    iter=0;
    while abs(c)<2 && iter<(num_iterations-1)
    do
        c=c*c+p;
        iter=iter+1
    end;
    coloring(iter/num_iterations)
end
