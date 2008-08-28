filter util_visualize_fft (image in)
  v = visualize_fft(in);
  g = gaussian_blur(v, 0.2, 0.2);
  p = v(xy);
  g0 = g(xy:[0,0]);
  f = gray(g0);
  rgba:[p[0]/f, p[1]/f, p[2]/f, 1]
end
