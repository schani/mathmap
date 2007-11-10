##
# Droste Effect Code
# Original code by breic ( www.flickr.com/photos/breic )
# Adapted by Pisco Bandito (www.flickr.com/photos/joshsommers)
# see also the flickr group: www.flickr.com/groups/escherdroste
# Version 5 - For Windows and Linux
# last modified: 3/23/2007
##

##Explanations:
##
# Roughly, p2 can be thought of as the "number of strands," while p1 is the "periodicity."
# See gadl's chessboard set (http://flickr.com/photos/gadl/sets/72157594305663827/) for a
# better explanation of the (p1, p2) parameters -- although he may have switched them!
# Mathematically, we rotate point (p1,p2) in the annular lattice in log coordinates to (0,1).
# The labels at escherdroste.math.leidenuniv.nl/index.php?menu=im&sub...
# agree with our notation here.
# There is a good explanation of the Droste-effect math at
# www.josleys.com/articles/printgallery.htm .
##
##
# Tiling can be based on transparency (if the input image is a tiff), or simply based on the
# radius. Using transparency, there can be protrusions between different annular layers.
# Tiling based on transparency, you can decide whether you want to look inward or
# outward from a transparent pixel. For example, with a frame you'll want to look inward,
# while for a flower you'll want to look outward.
##
##
# To avoid framing problems on the largest annulus when tiling based on transparency, look
# outside (StartingLevel) levels to see if something farther out should cover up this pixel
# Try setting to 0 to see framing errors; 1 should be sufficient unless you have three or more
# image layers contributing to some pixel (in which case set it to 2 or more). Larger values
# slow the code down, and may lead to floating point errors.
##

filter droste (image in,
               float r1: 0-2 (0.4),  #Inner Radius (must be less than r2)
               float r2: 0-2 (1),    #Outer Radius
               int periodicity: 2-20 (4),   #Periodicity
               float p2: 0-10 (1),   #Number of strands
               float zoom: 0-10 (1),
               float rotate: -3.1416-3.1416 (0),
               float x_origin: -1-1 (0),
               float y_origin: -1-1 (0),
               float x_shift: -1-1 (0),
               float y_shift: -1-1 (0),
               int starting_level: 0-100 (6),
               int number_of_levels: 0-100 (10),
               int level_frequency: 1-10 (1),
               bool no_transparency (1), bool external_transparency,
               bool mirror_effect, bool untwist)

    p1 = periodicity / 4;

    #For the mirror effect
    #Fiddle with this value depending on the values you have for p1 and p2 to get an alternating reflected effect
    MirrorCoefficient=1;

    # Miscellaneous useful variables
    true=1;
    false=0;
    epsilon=.01; # used to avoid hard comparisons
    imageX=W;
    imageY=H;
    minDimension=min(imageX, imageY);

    if zoom > 0 then
        zoom=(zoom*p1)/10;
    end;

    #User Values Defined
    tileBasedOnTransparency = !no_transparency;
    transparentPointsIn = !external_transparency;
    retwist=!untwist;

    ##
    # Droste-effect code starts here
    # Set Droste effect parameters
    ##
    alpha=atan(p2/p1*log(r2/r1)/(2*pi));
    f=cos(alpha);
    beta=f*exp(I*alpha);

    # the angle of rotation between adjacent annular levels
    angle = 2*pi*p1;

    if mirror_effect then
        angle=angle/MirrorCoefficient;
    end;

    if (0>p2) then
        angle = -angle;
    end;

    ##
    # Code to set up the viewport properly
    ##
    if (retwist) then
        xbounds=[-r2,r2];
        ybounds=[-r2,r2];
    else
        ybounds=[0,2.1*pi];
        xbounds=[-log(r2/r1), log(r2/r1)];
    end;

    xymiddle=ri:[0.5*(xbounds[0]+xbounds[1]),0.5*(ybounds[0]+ybounds[1])];
    xyrange=xy:[xbounds[1]-xbounds[0], ybounds[1]-ybounds[0]];
    aspectRatio=W/H;
    xyrange[0]=xyrange[1]*aspectRatio;
    xbounds=[xymiddle[0]-0.5*xyrange[0],xymiddle[0]+0.5*xyrange[0]];
    z=ri:[xbounds[0]+(xbounds[1]-xbounds[0])*(x+W/2)/W,ybounds[0]+(ybounds[1]-ybounds[0])*(y+H/2)/H];
    z[0] = z[0]-(x_shift);
    z[1] = z[1]-(y_shift);
    if (retwist) then # only allow for procedural zooming/scaling in the standard coordinates
        zinitial=z;
        z=xymiddle+(z-xymiddle)/zoom*exp(-I*rotate);
    else
        zinitial=r1*exp(z); # save these coordinates for drawing a frame later
        zinitial=zinitial*zoom*exp(I*rotate);
    end;

    ##
    # The Droste effect math all takes place over the next six lines.
    # All the rest of the code is for niceties.
    ##

    if (retwist) then
        z2=log(z/r1);
    else
        z2 = z;
    end;

    z=p1*z2/beta;
    z=r1*exp(z);

    ## End Droste effect math

    if (tileBasedOnTransparency && starting_level > 0) then
        if (!transparentPointsIn) then
            ratio=r2/r1*exp( I*angle);
        end;
        if ( transparentPointsIn) then
            ratio=r1/r2*exp(-I*angle);
        end;
        z=z*exp(starting_level*log(ratio));
    end;

    ##
    # When tiling based on transparency, color is accumulated into the colorSoFar variable,
    # while alphaRemaining tells how much remains for lower layers to contribute (initially 1,
    # finally 0).
    ##
    colorSoFar=rgba:[0,0,0,0];
    alphaRemaining=1;
    ix=minDimension/2*z[0];
    iy=minDimension/2*z[1];
    clr=in(xy:[ix + x_origin*W,iy + y_origin*H]);
    colorSoFar = colorSoFar + (clr*(alpha(clr)*alphaRemaining));
    alphaRemaining=alphaRemaining*(1-alpha(clr));
    sign=0;

    # do we need to look inward from the current point, or outward?

    if (tileBasedOnTransparency) then
        if ( transparentPointsIn && alphaRemaining > epsilon) then
            sign=-1;
        end;
        if (!transparentPointsIn && alphaRemaining > epsilon) then
            sign= 1;
        end;
    else
        radius=sqrt(z[0]*z[0]+z[1]*z[1]);
        if (r1 > radius) then
            sign=-1;
        end;
        if (radius > r2) then
            sign= 1;
        end;
    end;

    if (0 > sign) then ratio=r2/r1*exp( I*angle); end;
    if (sign > 0) then ratio=r1/r2*exp(-I*angle); end;

    if (level_frequency > 1) then
        ratio = exp(log(ratio)*level_frequency);
    end;

    ##
    # Iteratively move inward or outward, until
    # the point has radius r in [r1, r2), if tileBasedOnTransparency=false
    # or until alphaRemaining=0, if tileBasedOnTransparency=true
    # In the latter case, we accumulate color at each step
    ##
    iteration=starting_level;
    maxiteration=number_of_levels+starting_level-1;

    while (sign != 0 && maxiteration > iteration) do
        z2=z*ratio;
        z=z2;
        ix=minDimension/2*(z[0]) + x_origin*W;
        iy=minDimension/2*(z[1]) + y_origin*H;
        sign=0;
        if (tileBasedOnTransparency) then
            clr=in(xy:[ix,iy]);
            colorSoFar = colorSoFar + (clr*(alpha(clr)*alphaRemaining));
            alphaRemaining=alphaRemaining*(1-alpha(clr));
            if ( transparentPointsIn && alphaRemaining > epsilon) then sign=-1; end;
            if (!transparentPointsIn && alphaRemaining > epsilon) then sign= 1; end;
        else
            radius=sqrt(z[0]*z[0]+z[1]*z[1]);
            colorSoFar=in(xy:[ix,iy]);
            if (r1 > radius) then sign=-1; end;
            if (radius > r2) then sign= 1; end;
        end;
        iteration=iteration+1;
    end;

    colorSoFar

end
