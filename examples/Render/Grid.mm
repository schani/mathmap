filter grid (float width: 2-500 (100), float height: 2-500 (100),
             float thickness: 0-500 (5))
  xoff = width * ceil(X/width) + thickness/2;
  yoff = height * ceil(Y/height) + thickness/2;
  grayColor(if ((x+xoff)%width)<=thickness || ((y+yoff)%height)<=thickness then 0 else 1 end)
end
