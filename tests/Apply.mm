unit filter circle (float radius)
  grayColor(if r < radius then 1 else 0 end)
end

unit filter bla (float radius: 0-1.5 (1))
  clos = circle(radius);
  clos(xy)
end
