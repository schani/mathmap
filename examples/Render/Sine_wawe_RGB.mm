#RGB version of original sine wave
filter sine_waveRGB
(float wavelengthR: 0-.1 (0.01),
float wavelengthG: 0-.1 (0.02),
float wavelengthB: 0-.1 (0.03))
rgbColor((sin(r/wavelengthR+t*2*pi)*0.5+0.5),(sin(r/wavelengthG+t*2*pi)
*0.5+0.5),(sin(r/wavelengthB+t*2*pi)*0.5+0.5))
end 