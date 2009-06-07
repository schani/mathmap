filter render_voronoi_cells (float z: 0-32 (0))
  n = voronoi_cells([x, y, z]);
  grayColor(scale(n, -1, 1, 0, 1))
end