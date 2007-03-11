filter square_decompose (image in, float width: 1-200 (50), float skip: 0-200 (30))
    pd=width+skip;
    mx=pmod(x,pd);
    my=pmod(y,pd);
    if inintv(mx,width/2,width/2+skip) || inintv(my,width/2,width/2+skip) then
      rgba:[0,0,0,0]
    else
      px = if mx <= width/2 then
          x-floor(x/pd)*skip
        else
          x-(floor(x/pd)+1)*skip
        end;
      py = if my <= width/2 then
          y-floor(y/pd)*skip
        else
          y-(floor(y/pd)+1)*skip
        end;
      in(xy:[px,py])
    end
end
