# @title Gauss Normalized
# @tags kernels

filter org.mathmap.kernel_gauss_normalized (float phi: 0.00001-1)
  g = exp(-r*r/(2*phi*phi))/(2*pi*phi*phi);
  rgba:[g, g, g, g]
end
