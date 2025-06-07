#Filter Angular Transition Bands
#Nevit Dilmen 2009
#Select Image in user values
#Number of frames in settings

filter angular_transition_bands (image in1, image in2, int bands: 1-30
(10) )
band=2*pi/bands;
if a%band+t*band < band then
in1(xy)
else
in2(xy)
end
end 