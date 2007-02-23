filter sine_wave (float wavelength: 1-100)
    grayColor(sin(r/wavelength+t*2*pi)*0.5+0.5)
end
