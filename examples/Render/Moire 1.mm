filter moire1 ()
    q=t*2*pi;
    abs(rgba:[sin(r*50+q)+sin(15*a+q),
              sin(r*70+q)+sin(17*a+q),
              sin(r*90+q)+sin(19*a+q),2])*0.5
end
