filter comp_dissolve (image in0,
                      image in1,
                      float opacity: 0-1 (0.5))
  size = pixelSize(in0);
  size_max = max(size[0], size[1]);
  n = scale(noise([x, y, 0] * size_max),-1.4,1.4,0,1);
  if n <= opacity then in1(xy) else in0(xy) end
end
