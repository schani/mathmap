filter combine_half_convolve (image in, image mask,
                              bool copy_alpha (1))
  convolved = half_convolve(in, mask, copy_alpha);
  convolved(xy)
end
