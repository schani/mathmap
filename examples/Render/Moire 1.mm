# @title Moire 1
# @tags render

filter moire1 ()
    q=t*2*pi;
    abs(rgba:[sin(r*300+q)+sin(15*a+q),
              sin(r*400+q)+sin(17*a+q),
              sin(r*500+q)+sin(19*a+q),2])*0.5
end
