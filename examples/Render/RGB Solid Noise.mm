filter rgb_solid_noise (float granularity: 0-50 (10),
                        float rz: 0-10, float gz: 0-10, float bz: 0-10)
    g = granularity;
    nxy = xy/R*g;
    rgba:[noise([nxy[0],nxy[1],rz]),
          noise([nxy[0],nxy[1],gz]),
          noise([nxy[0],nxy[1],bz]),1]*0.5+0.5
end
