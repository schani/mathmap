#Filter Vertical Gate Transition
#Nevit Dilmen 2009
#Select Image in user values
#Number of frames in settings

filter vertical_gate_transition (image in1, image in2 )
if x*x > t then
in1(xy)
else
in2(xy)
end
end 