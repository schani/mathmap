# Simple Transition 1
# Nevit Dilmen 2009
# Select second image in User values
# Adjust time and frames in settings

filter Simple_transition1 (image in1, image in2)
lerp(t,in1(xy),in2(xy))
end
