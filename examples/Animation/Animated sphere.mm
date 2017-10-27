#Sphere Filter

#Originally by Herbert Poetzl

#Adapted for animation by Nathan deGargoyle

#Further Adaptation by Josh Sommers (Pisco Bandito)

# Badly edited by PhotoComiX to add a couple of effects



#RotateX_Start is the starting rotation on the X axis

#RotateY_Start is the starting rotation on the Y axis

#RotateZ_Start is the starting rotation on the Z axis



#Add_RotateX this value is added to RotateX_Start

#Add_RotateY this value is added i to #RotateY_Start

#Add_RotateZ this value is added to Start_RotationZ

#Negative values for "Add_Rotate" allow to invert direction as from 340° to 10° ,"flip" could do it much better
# then if by adding negative values the value of alfa, gamma or zeta become negative results willl be weird




#Multiply_X is the number of times to multply the rotation specified by RotateX_End.

#Multyply_Y is the number of times to multiply the rotation specified by RotateX_End.

#Multiply_Z is the number of times to multiply the rotation specified by RotateX_End.



#Mulltiply example:

#If you set Add_RotateX to 360 and Multiply_X to 10 the sphere will rotate 360 degrees on the X axis ten times.

#If you set Add_RotateX_ to 180 and Multiply_X to 10, then sphere will rotate 360 degrees on the X axis 5 times.



#Zoom_Start is the starting zoom

#Add_Zoom_ This value is added to the Zoom_Start value



#Background is the color to use for the background

#BackgroundOpacity is the opacity of the background, 0 for transparent, 100 for opaque. Your image must have an alpha channel for this to work.



############################################################

filter Sphere (image in,

int RotateX_Start: -360-360 (90),

int RotateY_Start: -360-360 (90),

#Starting from RotateY_Start =120 you will start with sphere fully invisible then it will start to appear
#When equal or more then 0 sphere will become fully visible

int RotateZ_Start: -360-360 (0),

int Add_RotateX: -360-360(0),

int Add_RotateY: -360-360(0),

int Add_RotateZ: -360-360(0),

int Multiply_X: 1-100(1),

int Multiply_Y: 1-100(1),

int Multiply_Z: 1-100(1),

int Zoom_Start: 1-300 (100),

int Add_Zoom: 1-300 (100),

bool Flip_X,

bool Flip_Y,

int BackgroundOpacity: 0-100 (0),

color background)



rd=min(X,Y)*((Zoom_Start/100)+(t*(Add_Zoom-Zoom_Start)/100));



if r>rd then

rgba:[background[0],background[1],background[2],BackgroundOpacity/100];

else



beta = (pi/180) * RotateX_Start + (pi/180)*(Add_RotateX*Multiply_X*t);

gamma = (pi/180) * RotateY_Start + (pi/180)*(Add_RotateY*Multiply_Y*t);

alpha = (pi/180) * RotateZ_Start + (pi/180)*(Add_RotateZ*Multiply_Z*t);



sa=sin(alpha);

sb=sin(beta);

ca=cos(alpha);

cb=cos(beta);



theta=a;

phi=acos(r/rd);



x0=cos(theta)*cos(phi);

y0=sin(theta)*cos(phi);

z0=sin(phi);

x1=ca*x0+sa*y0;

z1=(-sa*-sb*x0)+(ca*-sb*y0+cb*z0);





if z1 >= 0 || 1 then

y1=cb*-sa*x0+cb*ca*y0+sb*z0;

else

z1=z1-2*cb*z0;

y1=cb*-sa*x0+cb*ca*y0-sb*z0;

end;



theta1=atan(-x1/y1)+(if y1>0 then pi/2 else 3*pi/2 end);

phi1=asin(z1);



ix = ((theta1*1+gamma)%(2*pi)-pi)/pi*X;

iy = -phi1/(pi/2)*Y;



if Flip_X then

ix = -ix

end;



if Flip_Y then

iy= -iy

end;



in(xy:[ix,iy])

end



end 