# Gray to HSV (c) Nevit Dilmen 2009 
# http://start.at/nevit
filter Gray2_hsv (image in_hue) 
  toRGBA(hsva:[(gray(in_hue(xy))+ t)%1 , 1, 1, 1]) 
end 