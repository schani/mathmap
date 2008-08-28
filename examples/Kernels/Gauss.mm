filter kernel_gauss (float phi: 0.00001-1)
  g = exp(-r*r/(2*phi*phi));
  grayColor(g)
end
