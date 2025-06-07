#Filter Vertical Transition
#Nevit Dilmen 2009
#Select Image one and 2 in user values
#Number of frames in settings

filter vertical_transition (image in1, image in2 )
if (x+1)/2 < t then
in1(xy)
else
in2(xy)
end
end 