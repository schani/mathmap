filter moire1 ()
    q=t*2*pi;
    abs(rgba:[sin(r/4+q)+sin(15*a+q),
              sin(r/3.5+q)+sin(17*a+q),
              sin(r/3+q)+sin(19*a+q),2])*0.5
end
