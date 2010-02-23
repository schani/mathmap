# @title Disco
# @tags render

filter org.mathmap.disco (int red_wavelength: 1-50 (10),  int green_wavelength: 1-50 (15),
              int blue_wavelength: 1-50 (20), float zoom: 0-3000 (500))
    rl=red_wavelength;
    gl=green_wavelength;
    bl=blue_wavelength;
    q=t*2*pi;
    rz=r*zoom;
    abs(rgba:[sin(rz/rl+q)+sin(a*rl+q),
              sin(rz/gl+q)+sin(a*gl+q),
              sin(rz/bl+q)+sin(a*bl+q),2])
end
