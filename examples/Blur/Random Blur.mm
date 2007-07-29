unit filter random_blur (unit image in, float radius: 0-1 (0.01), int iters: 2-100 (5))
  i=0;
  sum = grayColor(0);
  while i < iters do
    pos = xy + xy:[rand(-radius, radius), rand(-radius, radius)];
    sum = sum + in(pos);
    i = i + 1;
  end;
  sum / iters
end
