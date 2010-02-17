# @title Visualize Sum
# @tags utilities

filter visualize_sum (image in)
  p = in(xy);
  g = sum(p[0..2]) / 3;
  mr = if g > 0 then
         clamp(g, 0, 1)
       else
         0
       end;
  mb = if g < 0 then
         clamp(-g, 0, 1)
       else
         0
       end;
  mg = if abs(g) > 1 then
         atan((abs(g)-1)*(pi/2))/(pi/2)
       else
         0
       end;
  rgba:[mr, mg, mb, 1]
end