filter combine_convolve (image in, image kernel,
                         bool copy_alpha (1))
  convolved = convolve(in, kernel, 1, copy_alpha);
  convolved(xy)
end