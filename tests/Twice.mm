filter twice (image in)
  in(xy) * 2
end

filter half (image in)
  in(xy) / 2
end

filter main (image in)
  half(twice(in), xy)
end