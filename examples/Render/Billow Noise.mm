# @title Billow Noise
# @tags render

filter billow_noise (int num_octaves: 1-32 (5),
                     float persistence: 0-1 (0.5),
                     float lacunarity: 0-10 (2),
                     float z: 0-32 (0))
  n = noiseBillow(num_octaves, persistence, lacunarity, [x, y, z]);
  grayColor(scale(n, -1, 1, 0, 1))
end
