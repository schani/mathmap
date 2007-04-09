unit filter grid (float width: 0-1 (0.2), float height: 0-1 (0.2),
                  float thickness: 0-1 (0.02))
  xoff = width * ceil(X/width) + thickness/2;
  yoff = height * ceil(Y/height) + thickness/2;
  grayColor(if ((x+xoff)%width)<=thickness || ((y+yoff)%height)<=thickness then 0 else 1 end)
end
