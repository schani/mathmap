filter combine_half_convolve (image in, image mask)
  convolved = half_convolve(in, mask);
  convolved(xy)
end
