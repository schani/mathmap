filter sine_wave (float wavelength: 0-1 (0.02))
    grayColor(sin(r/wavelength+t*2*pi)*0.5+0.5)
end
