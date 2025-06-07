#Angular Divisions Nevit Dilmen 2009
filter angular_divisions (float divisions: 1-36 (6))
div=floor(divisions);
c=(2*pi)/div;
p=a%c;
n=scale (p, 0, c, 0, 1);
grayColor (n)
end 