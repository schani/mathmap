#Filter Radial Transition Bands
#Nevit Dilmen 2009
#Select Image  in user values
#Number of frames in settings

filter radial_transition_bands (image in1, image in2, int bands: 2-20
(5)  )
band=1/bands;
if r%band+((1-t)*band) > band then
in1(xy)
else
in2(xy)
end
end 