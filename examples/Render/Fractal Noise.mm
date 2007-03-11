filter fractal_noise (float z: 0-10 (1.0), int depth: 1-10 (5),
                      float persistence: 0-1 (0.6), float granularity: 0-50 (7.0))
    pers = persistence;
    g = granularity;
    nxy = xy / R * g;
    xyz = [nxy[0],nxy[1],z];
    i = 1;
    sum = 0;
    max = 0;
    while i < depth + 1 do
        sum = sum + noise(xyz*i) * (pers^i);
        max = max + (pers ^ i);
        i = i + 1
    end;
    grayColor(sum / (max * 2) + 0.5)
end
