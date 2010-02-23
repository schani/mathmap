# @title IFS Iterative
# @tags map

# Code for a simple IFS effect in mathmap

filter org.mathmap.IFS (image in,
            float scaling: 1-20 (1.3), float rotation: 0-6.283 (0.2),
            float translate_x: -1-1 (0.1), float translate_y: -1-1 (0.3),
            int num_iterations: 1-50 (10))

  trans = xy:[translate_x,translate_y];

  final_ra = vra = ra;

  iteration = 0;
  while iteration < num_iterations do
    if abs((toXY(vra))[0])<X && abs((toXY(vra))[1])<Y then
        final_ra = vra;
    end;
    vra = vra * ra:[scaling,1] + ra:[0,rotation];
    vra = toRA(toXY(vra) + trans);
    iteration = iteration + 1
  end;

  in(final_ra)

end
