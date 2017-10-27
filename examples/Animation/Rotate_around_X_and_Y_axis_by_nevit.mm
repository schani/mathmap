#Rotate around X Axis (c) Nevit Dilmen 2009
filter Rotate_X_axis (image in)s=cos(t*2*pi);
in (xy:[x,y/s])
end

#Rotate around Y Axis (c) Nevit Dilmen 2009
filter Rotate_Y_axis (image in)s=cos(t*2*pi);
in (xy:[x/s,y])
end