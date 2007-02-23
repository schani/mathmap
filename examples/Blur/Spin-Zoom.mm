# by tomr@aceldama.com
filter spin_zoom (image in, float zoom: 0-1, float angle: 0-6.2831853, int samples: 2-64,
                  curve zoom_at_r, curve position_at_t)
    zfact = (zoom_at_r(r/X)*zoom*X)/samples;
    rfact = (position_at_t(t)*angle)/samples;
    sample = 0;
    total = rgba:[0,0,0,0];
    while (sample < samples) do
        total = total + in(ra + ra:[zfact, rfact] * sample);
        sample = sample+1
    end;
    total/samples
end
