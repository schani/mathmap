# @title Flag
# @tags distorts

# Simone Demmel <neko@greenie.muc.de>
filter flag (image in, float amp: 0-1 (0.05),
             float len: 0-50 (5), int speed: 1-5 (1))
  in(xy:[x,y+amp*sin(pi*len/X*(-x+t*2*speed/len))])
end