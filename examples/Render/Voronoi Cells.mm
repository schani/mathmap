# @title Voronoi Cells
# @tags render

filter render_voronoi_cells (float z: 0-32 (0))
  n = voronoiCells([x, y, z]);
  grayColor(scale(n, -1, 1, 0, 1))
end
