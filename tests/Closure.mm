filter circle (float radius)
  grayColor(if r < radius then 0.5-t else 0 end)
end

filter combine (image i1, image i2)
  i1(xy) * i2(xy)
end

filter bla (image in, float radius: 0-1.5 (1))
  combine(in, circle(radius), xy)
end
