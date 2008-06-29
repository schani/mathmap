stretched filter gauss_ident (stretched image in)
  in(xy)
end

stretched filter gauss_blur (stretched image in, float dev: 0 - 0.5)
  rendered = render(gauss_ident(in));
  size = pixelSize(in);
  maxdim = max(size[0], size[1]);
  blurred = gaussian_blur(rendered, dev * maxdim / size[0], dev * maxdim / size[1]);
  blurred(xy)
end
