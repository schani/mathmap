filter mandelbrot (float zoom: 0-2 (1.5), float xoff: -1-2 (0.5), float yoff: -1-1 (0),
                   float pj: -2-2 (0), float pk: -2-2 (0),
                   float c1: -2-2 (0), float ci: -2-2 (0),
                   float cj: -2-2 (0), float ck: -2-2 (0),
                   int num_iterations: 2-256 (32), gradient coloring)
"Draws the well-known Mandelbrot fractal."
    pc=ri:(xy*zoom-xy:[xoff,yoff]);
    p=quat:[pc[0],pc[1],pj,pk];
    c=quat:[c1,ci,cj,ck];
    iter=0;
    while abs(c)<2 && iter<(num_iterations-1)
    do
        c=c*c+p;
        iter=iter+1
    end;
    coloring(iter/num_iterations)
end
