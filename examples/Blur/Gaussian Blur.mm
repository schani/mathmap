stretched filter gauss_ident (stretched image in, float factor)
  in(xy * factor)
end

stretched filter gauss_blur (stretched image in, float dev: 0 - 0.5,
                             float factor: 1-2)
  rendered = render(gauss_ident(in, factor));
  size = pixelSize(in);
  maxdim = max(size[0], size[1]);
  blurred = gaussian_blur(rendered, dev * maxdim / size[0], dev * maxdim / size[1]);
  blurred(xy)
end
