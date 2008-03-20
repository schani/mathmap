pixel filter life (pixel image in)
"This effect is just included to demonstrate the limits of possibility with MathMap.  It calculates one round of John Conway's `Game of Life'."
    num=(gray(in(xy+xy:[-1,-1]))>0.5)+
        (gray(in(xy+xy:[-1,0]))>0.5)+
        (gray(in(xy+xy:[-1,1]))>0.5)+
        (gray(in(xy+xy:[0,-1]))>0.5)+
        (gray(in(xy+xy:[0,1]))>0.5)+
        (gray(in(xy+xy:[1,-1]))>0.5)+
        (gray(in(xy+xy:[1,0]))>0.5)+
        (gray(in(xy+xy:[1,1]))>0.5);
    val=gray(in(xy))>0.5;
    rgba:[0,0,0,1]+rgba:[1,1,1,0]*if num==2 then val else if num==3 then 1 else 0 end end
end
