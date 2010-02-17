# @title Zoom-Twist
# @tags blur

# by tomr@aceldama.com
filter zoom_twist (image in, int samples: 4-64 (10),
                   float zoom: 0-1 (0.2), float twist: 0-6.2831853,
                   curve twist_at_t, curve twist_at_r,
                   curve zoom_at_t, curve zoom_at_r,
                   curve sampling_shape)
"This expression can do quite amazing effects, but you'll have to figure it out yourself."
    tfact = twist * (twist_at_t(t) - 0.5);
    zfact = (zoom_at_t(t)-0.5) * (zoom_at_r(r)-0.5) * zoom;
    total = rgba:[0,0,0,0];
    for sample = 0 .. samples-1 do
        radj = zfact * (sampling_shape(sample/samples)-0.5);
        sr = r + radj;
        sa = a + (twist_at_r(radj)-0.5) * tfact;
        total = total + in(ra:[sr, sa])
    end;
    total/samples
end
