#Filter Radial Transition
#Nevit Dilmen 2009
#Select Image in user values
#Number of frames in settings

filter radial_transition (image in1, image in2 )
if r/R < t then
in1(xy)
else
in2(xy)
end
end 