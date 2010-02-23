# @title Mandelbrot
# @tags render

filter org.mathmap.render_mandelbrot (float pj: -2-2 (0), float pk: -2-2 (0),
                          float c1: -2-2 (0), float ci: -2-2 (0),
                          float cj: -2-2 (0), float ck: -2-2 (0),
                          int num_iterations: 2-256 (32))
"Draws the well-known Mandelbrot fractal."
    pc=ri:xy;
    p=quat:[pc[0],pc[1],pj,pk];
    c=quat:[c1,ci,cj,ck];
    iter=0;
    while abs(c)<2 && iter<(num_iterations-1)
    do
        c=c*c+p;
        iter=iter+1
    end;
    grayColor(iter/num_iterations)
end
