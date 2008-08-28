stretched filter blur_gauss (stretched image in, float dev: 0 - 0.5)
  size = pixelSize(in);
  maxdim = max(size[0], size[1]);
  blurred = gaussian_blur(in, dev * maxdim / size[0], dev * maxdim / size[1]);
  blurred(xy)
end
