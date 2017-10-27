# Nevit Dilmen 2009
# Select second image in User values
# Adjust time and frames in settings
filter Simple_transition (image in1, image in2)
j=((t*2)-1);
m=j*j;
    lerp(m,in1(xy),in2(xy))
end
