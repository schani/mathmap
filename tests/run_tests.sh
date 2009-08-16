#!/bin/bash

OUTFILE=/tmp/mathtest_$$.png

# Some versions of perceptualdiff are broken - they return 0 on
# failure.  Test which one this is and then behave accordingly.

if perceptualdiff marlene.png marlene.png ; then
    PDIFF_BROKEN=0
else
    PDIFF_BROKEN=1
fi

run_test () {
    SCRIPT=$1
    REFERENCE=$2
    INPUT_ARGS=$3

    echo "Running $SCRIPT"

    if [ ! -f "$REFERENCE" ] ; then
	echo "Reference file doesn't exist - creating it."
	../mathmap -i -f "$SCRIPT" $INPUT_ARGS "$REFERENCE" >&/dev/null
	if [ ! -f "$REFERENCE" ] ; then
	    echo "Error: MathMap didn't produce an output image."
	    exit 1
	fi
    fi

    rm -f "$OUTFILE"
    ../mathmap -i -f "$SCRIPT" $INPUT_ARGS "$OUTFILE" >&/dev/null
    if [ ! -f "$OUTFILE" ] ; then
	echo "Error: MathMap did not produce an output image."
	exit 1
    fi

    if [ $PDIFF_BROKEN -eq 0 ] ; then
	if perceptualdiff "$OUTFILE" "$REFERENCE" -fov 85 -threshold 50 ; then
	    true
	else
	    echo "Error: Output image $OUTFILE doesn't match reference $REFERENCE."
	    exit 1
	fi
    else
	if perceptualdiff "$OUTFILE" "$REFERENCE" -fov 85 -threshold 50 ; then
	    echo "Error: Output image $OUTFILE doesn't match reference $REFERENCE."
	    exit 1
	fi
    fi
}

run_render_test () {
    run_test "$1" "$2" "-s 256x256 $3"
}

run_modify_test () {
    run_test "$1" "$2" "-Din=marlene.png $3"
}



run_render_test Apply.mm apply.png
run_modify_test Circle.mm circle.png
run_modify_test Closure.mm closure.png
run_modify_test Twice.mm twice.png


run_modify_test "../examples/Blur/Mosaic.mm" blur_mosaic.png
run_modify_test "../examples/Blur/Radial Mosaic.mm" blur_radial_mosaic.png
run_modify_test "../examples/Blur/Radial Mosaic Bertl Edition.mm" blur_radial_mosaic_bertl.png
run_modify_test "../examples/Blur/Gaussian Blur.mm" blur_gaussian_blur.png "-Ddev=0.1"
run_modify_test "../examples/Blur/Spin-Zoom.mm" blur_spin_zoom.png
run_modify_test "../examples/Blur/Zoom-Twist.mm" blur_zoom_twist.png

# Colors->Alpha to Gray
# Colors->Auto BW
run_modify_test "../examples/Colors/Brightness.mm" colors_brightness.png "-Dbrightness=0.2"
run_modify_test "../examples/Colors/Colorify.mm" colors_colorify.png
# Colors->Combine HSV
# Colors->Combine RGB
run_modify_test "../examples/Colors/Contrast.mm" colors_contrast.png "-Dcontrast=0.2"
run_modify_test "../examples/Colors/Desaturate.mm" colors_desaturate.png
# Colors->Gamma Correction
run_modify_test "../examples/Colors/Invert.mm" colors_invert.png
# Colors->Local Contrast
run_modify_test "../examples/Colors/Lomoize.mm" colors_lomoize.png
run_modify_test "../examples/Colors/Monochrome Channel Mixer.mm" colors_monochrome_channel_mixer.png
# Colors->Pidgin Gamma Correction
# Colors->Set Alpha
run_modify_test "../examples/Colors/Threshold.mm" colors_threshold.png
run_modify_test "../examples/Colors/Tint.mm" colors_tint.png "-Dhue=0.2 -Dsaturation=0.2"

# Combine->*

run_modify_test "../examples/Distorts/Bilinear Interpolation.mm" distorts_bilinear_interpolation.png "-Da3=0.2 -Db2=-0.2"
run_modify_test "../examples/Distorts/Circular Slice.mm" distorts_circular_slice.png
# Distorts->Curve Bend
run_modify_test "../examples/Distorts/Defish.mm" distorts_defish.png
run_modify_test "../examples/Distorts/Enhanced Pond.mm" distorts_enhanced_pond.png
run_modify_test "../examples/Distorts/Fisheye.mm" distorts_fisheye.png
run_modify_test "../examples/Distorts/Flag.mm" distorts_flag.png
run_modify_test "../examples/Distorts/Inverse Lambert Azimuthal Projection.mm" distorts_inverse_lambert_azimuthal.png
run_modify_test "../examples/Distorts/Jitter.mm" distorts_jitter.png
run_modify_test "../examples/Distorts/Mercator.mm" distorts_mercator.png
run_modify_test "../examples/Distorts/Miniplanet.mm" distorts_miniplanet.png
# Distorts->Peel Animation
run_modify_test "../examples/Distorts/Pond.mm" distorts_pond.png
run_modify_test "../examples/Distorts/Radial Eversion.mm" distorts_radial_eversion.png
run_modify_test "../examples/Distorts/Rotation.mm" distorts_rotation.png "-Dtheta=0.1 -Dpsi=0.7 -Dradius=100"
run_modify_test "../examples/Distorts/Sea.mm" distorts_sea.png
run_modify_test "../examples/Distorts/Slice.mm" distorts_slice.png
run_modify_test "../examples/Distorts/Sphere.mm" distorts_sphere.png
run_modify_test "../examples/Distorts/Square.mm" distorts_square.png
run_modify_test "../examples/Distorts/Stereographic Projection.mm" distorts_stereographic_projection.png
run_modify_test "../examples/Distorts/Twirl.mm" distorts_twirl.png
run_modify_test "../examples/Distorts/Wave.mm" distorts_wave.png

# Edge Detect->Gauss Blur Edge Detect
# Edge Detect->Random Edge Detect

run_modify_test "../examples/Geometry/Rotate.mm" geometry_rotate.png "-Dangle=1"
run_modify_test "../examples/Geometry/Scale.mm" geometry_scale.png "-Dsx=1.5 -Dsy=-0.7"
run_modify_test "../examples/Geometry/Shear.mm" geometry_shear.png "-Dax=0.5 -Day=-1.5"
run_modify_test "../examples/Geometry/Translate.mm" geometry_translate.png "-Ddx=0.5 -Ddy=-0.7"
run_modify_test "../examples/Geometry/Zoom.mm" geometry_zoom.png "-Dfactor=1.2"

run_render_test "../examples/Kernels/Gauss.mm" kernels_gauss.png "-Dphi=0.2"
run_render_test "../examples/Kernels/Gauss Normalized.mm" kernels_gauss_normalized.png "-Dphi=0.35"

# Map->Displace
run_modify_test "../examples/Map/Droste.mm" map_droste.png
run_modify_test "../examples/Map/IFS Functional.mm" map_ifs_functional.png
run_modify_test "../examples/Map/IFS Iterative.mm" map_ifs_iterative.png
# Map->Make Seamless
run_modify_test "../examples/Map/Mugl.mm" map_mugl.png
# Map->Radial Displace
run_modify_test "../examples/Map/Sphere.mm" map_sphere.png
run_modify_test "../examples/Map/Sphere with Reflection.mm" map_sphere_with_reflection.png
run_modify_test "../examples/Map/Square Decompose.mm" map_square_decompose.png
run_modify_test "../examples/Map/Tile.mm" map_tile.png

# Math->*

run_modify_test "../examples/Noise/Life.mm" noise_life.png
# Noise->Scatter

run_render_test "../examples/Render/Billow Noise.mm" render_billow_noise.png
run_modify_test "../examples/Render/Darts Board.mm" render_darts_board.png
run_render_test "../examples/Render/Disco.mm" render_disco.png
run_modify_test "../examples/Render/Domain Coloring.mm" render_domain_coloring.png
run_render_test "../examples/Render/Fancy Mandelbrot.mm" mandelbrot.png
run_render_test "../examples/Render/Fractal Noise.mm" render_fractal_noise.png
run_render_test "../examples/Render/Gray.mm" render_gray.png "-Dvalue=0.2"
run_render_test "../examples/Render/Grid.mm" render_grid.png
run_render_test "../examples/Render/Grid Based Domain Coloring.mm" render_grid_based_domain_coloring.png
run_render_test "../examples/Render/HSV Domain Coloring.mm" render_hsv_domain_coloring.png
run_render_test "../examples/Render/Mandelbrot.mm" render_mandelbrot.png
run_render_test "../examples/Render/Moire 1.mm" render_moire_1.png
run_render_test "../examples/Render/Moire 2.mm" render_moire_2.png
run_render_test "../examples/Render/Perlin Noise.mm" render_perlin_noise.png
# Render->Plot function
run_render_test "../examples/Render/RGB Solid Noise.mm" render_rgb_solid_noise.png "-Drz=0.4 -Dgz=1"
run_render_test "../examples/Render/Ridged Multi Noise.mm" render_ridged_multi_noise.png
run_render_test "../examples/Render/Sine Wave.mm" render_sine_wave.png
run_render_test "../examples/Render/Spiral.mm" render_spiral.png
run_render_test "../examples/Render/Voronoi Cells.mm" render_voronoi_cells.png
run_render_test "../examples/Render/Weird Black and White Texture.mm" render_weird_black_and_white_texture.png

# Time->*

run_modify_test "../examples/Utilities/Ident.mm" utilities_ident.png
run_modify_test "../examples/Utilities/Render.mm" utilities_render.png
run_modify_test "../examples/Utilities/Visualize FFT.mm" utilities_visualize_fft.png
run_modify_test "../examples/Utilities/Visualize Magnitude.mm" utilities_visualize_magnitude.png
run_modify_test "../examples/Utilities/Visualize Sum.mm" utilities_visualize_sum.png
