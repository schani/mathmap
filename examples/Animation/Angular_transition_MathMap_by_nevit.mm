#Filter Angular Transition
#Nevit Dilmen 2009
#Select Image one and 2 in user values
#Number of frames in settings

filter angular_transition (image in1, image in2 )
if a < t*2*pi then
in1(xy)
else
in2(xy)
end
end