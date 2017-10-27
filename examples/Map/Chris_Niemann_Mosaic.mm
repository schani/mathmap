filter christoph_niemann (image in, float zoom: 1-10 (2),
                          int nx: 2-20 (8), int ny: 2-20 (8))
  n = xy:[nx, ny];
  WHs = WH/n;
  square = (xy + XY) / WHs;
  square = xy:[floor(square[0]), floor(square[1])];
  sxy = (xy + XY) % WHs;
  subxy = (WHs - (WHs*zoom - WHs) / (n - 1)) * square;
  in(subxy + sxy * zoom - XY)
end 