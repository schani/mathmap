filter rel2ellv3(image in)
xprime=x/sqrt(1-4*(y/H)*(y/H));
yprime=y/sqrt(1-4*(x/W)*(x/W));
in(xy:[xprime,yprime])
end 
