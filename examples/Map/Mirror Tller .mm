stretched filter mirrortile (stretched image in, int n: 1-10 (3))
  if (floor(((x+1)*n)/W%2-1)) then
    newx = ((x+1)*n)%2-1;
  else
    newx = ((W-x+1)*n)%2-1;
  end;
  if (floor(((y+1)*n)/H%2-1)) then
    newy = ((y+1)*n)%2-1;
  else
    newy = ((H-y+1)*n)%2-1;
  end;
  in(xy:[newx,newy])
end