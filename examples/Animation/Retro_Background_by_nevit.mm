
#Retro Background (c) Nevit Dilmen 2009
#Don't forget to select two colors in User Values

filter Retro_BG (float divisions: 1-36 (6), color One, color Two)
div=floor(divisions);
c=(2*pi)/div;
s=scale (t,0,1,0,c );
p=(a+s)%c;
n=scale (p, 0, c, 0, 1);
 if n< 0.5 then
 One
 else
 Two
 end
end