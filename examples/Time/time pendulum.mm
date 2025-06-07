#Time Pendulum Nevit Dilmen
# This filter is aimed to be like invert t
# and to be used in composer
# It swings t back and forward like a pendulum
filter time_pendulum (image in)
  in(xy, sin(t*pi))
end 