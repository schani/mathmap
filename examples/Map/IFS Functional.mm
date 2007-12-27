unit filter rotate (unit image in, float angle: 0-6.29)
  in(ra:[r,a+angle])
end

unit filter translate (unit image in,
                       float dx: -2-2, float dy: -2-2)
  in(xy-xy:[dx,dy])
end

unit filter zoom (unit image in, float s)
  in(xy/s)
end

unit filter rottrans (unit image in,
                      float angle: 0-6.29,
                      float dx: -2-2, float dy: -2-2)
  translate(rotate(in, angle), dx, dy, xy)
end

unit filter comp_sover (unit image under, unit image over)
  op = over(xy);
  oa = alpha(op);
  op * oa + under(xy) * (1 - oa)
end

unit filter ifs (unit image in,
                 float factor: 0-1 (0.7),
                 float angle: 0-6.29 (0.2),
                 float dx: -2-2 (0.15), float dy: -2-2 (-0.2),
                 int depth: 1-32 (8))
  if depth < 2 then
    in(xy)
  else
    comp_sover(in,
               rottrans(zoom(ifs(in, factor, angle, dx, dy, depth - 1), factor),
                        angle, dx, dy),
               xy)
  end
end

