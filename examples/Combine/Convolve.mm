filter combine_convolve (image in, image kernel)
  convolved = convolve(in, kernel, 1);
  convolved(xy)
end