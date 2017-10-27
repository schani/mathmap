

# filter spiral_rgb (c) Nevit Dilmen 2009

filter spiral_rgb (float red_rotations: 0-20 (1),float green_rotations: 0-20 (3),float blue_rotations: 0-20 (5) )

RRR=sin(r*red_rotations*pi*2-a+t*2*pi)*0.5+0.5;

GGG=sin(r*green_rotations*pi*2-a+t*2*pi)*0.5+0.5;

BBB=sin(r*blue_rotations*pi*2-a+t*2*pi)*0.5+0.5;

rgbColor(RRR,GGG,BBB)

end 
