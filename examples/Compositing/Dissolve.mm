pixel filter comp_dissolve (pixel image in0,
                            pixel image in1,
                            float opacity: 0-1 (0.5))
  n = scale(noise([x, y, 0]),-0.7,0.7,0,1);
  if n <= opacity then in1(xy) else in0(xy) end
end
