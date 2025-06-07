filter rel2ellv3(image in)
xprime=x/sqrt(1-4*(y/H)*(y/H));
yprime=y/sqrt(1-4*(x/W)*(x/W));
in(xy:[xprime,yprime])
end 


filter __composer_filter__2 (image rel2ellv3_in)
    rel2ellv3_out = rel2ellv3(rel2ellv3_in);
    rel2ellv3_2_out = rel2ellv3(rel2ellv3_out);
    rel2ellv3_2_out(xy)
end
