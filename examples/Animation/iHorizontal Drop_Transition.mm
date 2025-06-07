#Filter Horizontal Drop Transition
#Nevit Dilmen 2009
#Select Image  in user values
#Number of frames in settings

filter horizontal_drop_transition (image in1, image in2 )
if (y+1)/2 < (1-t) then
in1(xy)
else
in2(xy)
end
end 