# @title Random Blur
# @tags blur

filter random_blur (image in, float radius: 0-1 (0.01), int iters: 2-100 (5))
  sum = grayColor(0);
  for i = 1 .. iters do
    pos = xy + xy:[rand(-radius, radius), rand(-radius, radius)];
    sum = sum + in(pos)
  end;
  sum / iters
end
