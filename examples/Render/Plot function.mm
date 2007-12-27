#########
#
# Function graph plotter
# Hans Lundmark
# 2002-04-15
#
# halun@mai.liu.se
# www.mai.liu.se/~halun/
#
#
# Make sure your image has an alpha channel!
#
# (Note: Jump discontinuities will give vertical
# lines, sorry about that...)
#
# Use the 'oversampling' option to get a nice
# smooth curve.
# (For a conventional curve, use thickness = 0.5,
# sampdist = 0.5, and no oversampling.)
#
#########

pixel filter function_plotter ()

    #########
    #
    # Various parameters.
    #
    # NOTE:
    # Which function to plot is specified
    # further down in the code!
    #
    # Curve features.
    #   The parameter 'thickness' affects the thickness
    #   in the y direction only.
    #   To make the curve thicker in the x direction,
    #   increase 'sampdist'.
    #   For functions which vary quickly,
    #   increase 'samples'.
    #   (To plot isolated points, set samples = 0.)
    #
      thickness = 1.0 ;
      samples =   1 ;
      sampdist =  1.0 ;
    #
    # Plotting range.
    #   Set y0mid = 1 or 0 to interpret y0 as y_midpoint
    #   or y_minimum, respectively.
    #
      xrange = [ -1.0 , 1.0 ] * 2 ;
      yscale = 1.0 ;
      y0 =     0.0 ;
      y0mid=   1 ;
    #
    #
    #########
    
    dy = yscale * (xrange[1]-xrange[0]) * Y / X; yrange = [0.0,dy]+y0; if y0mid then yrange=yrange-dy/2 end;
    
    #########
    #
    # To set 'yrange' directly,
    # overriding the setting using y0 and yscale,
    # uncomment the following line.
    #
      # yrange=[ -1.0 , 1.0 ];
    #
    #
    #########
    
    
    low=1000000.0; high=-1000000.0;
    dx=-samples; while dx<=samples do q=scale( (x+dx*sampdist/samples)/X,-1,1,xrange[0],xrange[1] );
    
    #########
    #
    # Specify which function to plot.
    #
    # We write it as function of 'q',
    # since 'x' is used by MathMap already.
    #
      f = 0.6*sin(2*q)-0.3*cos(7*q) ;
    #
    #########
    
    low = min(low,f); high =max(high,f); dx = dx+1 end;
    if inintv( y, scale(low,yrange[0],yrange[1],-Y,Y)-thickness,scale(high,yrange[0],yrange[1],-Y,Y)+thickness) then
        rgba:[0.0,0.0,0.0,1.0]
    else
        rgba:[0.0,0.0,0.0,0.0]
    end
end
