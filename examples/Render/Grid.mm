# @title Grid
# @tags render

filter org.mathmap.grid (float width: 0-1 (0.2), float height: 0-1 (0.2),
             float thickness: 0-1 (0.02))
  nxy = abs(xy) + thickness/2;
  grayColor(if (nxy[0]%width)<=thickness || (nxy[1]%height)<=thickness then 0 else 1 end)
end