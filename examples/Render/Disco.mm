filter disco (int red_wavelength: 1-50 (10),  int green_wavelength: 1-50 (15),
              int blue_wavelength: 1-50 (20))
    rl=red_wavelength;
    gl=green_wavelength;
    bl=blue_wavelength;
    q=t*2*pi;
    abs(rgba:[sin(r/rl+q)+sin(a*rl+q),
              sin(r/gl+q)+sin(a*gl+q),
              sin(r/bl+q)+sin(a*bl+q),2])
end
