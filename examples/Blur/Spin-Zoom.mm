# @title Spin-Zoom
# @tags blur

# by tomr@aceldama.com
filter org.mathmap.spin_zoom (image in, float zoom: 0-1 (0.2), float angle: 0-6.2831853,
                  int samples: 2-64 (10), curve zoom_at_r, curve position_at_t)
"This is quite an involved expression.  It `blur-zooms' an image and can create an animation which involves rotating the input image.  Try changing the `zoom' and `angle', then play around with the time slider."
    zfact = (zoom_at_r(r)*zoom)/samples;
    rfact = (position_at_t(t)*angle)/samples;
    total = rgba:[0,0,0,0];
    for sample = 0 .. samples-1 do
        total = total + in(ra + ra:[zfact, rfact] * sample)
    end;
    total/samples
end
