#Filter Vertical Curtain Transition Bands
#Nevit Dilmen 2009
#Select Image in user values
#Number of frames in settings

filter vertical_curtain_transition_bands (image in1, image in2, int
bands:1-25 (5) )
band=1/(bands+1);
if ((x+1)/2)%band+t*band > band then
in1(xy)
else
in2(xy)
end
end 