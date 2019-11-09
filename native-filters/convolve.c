/* -*- c -*- */

/*
 * convolve.c
 *
 * MathMap
 *
 * Copyright (C) 2008-2009 Mark Probst
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <complex.h>
#include <fftw3.h>
#include <string.h>

#include "../drawable.h"
#include "../mmpools.h"

#include "native-filters.h"

static void
copy (double *dest, float *src, int n)
{
    int i;

    for (i = 0; i < n; ++i)
	dest[i] = src[i * NUM_FLOATMAP_CHANNELS];
}

static double
copy_and_add (double *dest, float *src, int n)
{
    int half;

    if (n <= 0)
	return 0.0;
    if (n == 1)
	return dest[0] = src[0];
    if (n == 2)
    {
	double d1, d2;

	d1 = dest[0] = src[0];
	d2 = dest[1] = src[NUM_FLOATMAP_CHANNELS];

	return d1 + d2;
    }

    half = n / 2;
    return copy_and_add(dest, src, half)
	+ copy_and_add(dest + half, src + half * NUM_FLOATMAP_CHANNELS, n - half);
}

CALLBACK_SYMBOL
image_t*
native_filter_convolve (mathmap_invocation_t *invocation, userval_t *args, mathmap_pools_t *pools)
{
    native_filter_cache_entry_t *cache_entry;
    image_t *in_image = args[0].v.image;
    image_t *filter_image = args[1].v.image;
    gboolean normalize = args[2].v.bool_const != 0.0;
    gboolean copy_alpha = args[3].v.bool_const != 0.0;
    image_t *out_image;
    double *fftw_in;
    fftw_complex *image_out, *filter_out;
    fftw_plan in_plan, filter_plan, inverse_plan;
    int i, n, nhalf, cn, channel, num_channels;

    cache_entry = invocation_lookup_native_filter_invocation(invocation, args, &native_filter_convolve);
    if (cache_entry->image != NULL)
	return cache_entry->image;

    if (in_image->type != IMAGE_FLOATMAP)
	in_image = render_image(invocation, in_image,
				invocation->render_width, invocation->render_height, pools, TRUE);
    if (filter_image->type != IMAGE_FLOATMAP
	|| filter_image->pixel_width != in_image->pixel_width
	|| filter_image->pixel_height != in_image->pixel_height)
	filter_image = render_image(invocation, filter_image,
				    in_image->pixel_width, in_image->pixel_height, pools, TRUE);

    out_image = floatmap_alloc(in_image->pixel_width, in_image->pixel_height, &invocation->pools);

    n = in_image->pixel_height * in_image->pixel_width;
    nhalf = in_image->pixel_width * (in_image->pixel_height / 2) + in_image->pixel_width / 2;
    cn = in_image->pixel_height * (in_image->pixel_width / 2 + 1);

    fftw_in = fftw_malloc(sizeof(double) * n);
    image_out = fftw_malloc(sizeof(fftw_complex) * cn);
    filter_out = fftw_malloc(sizeof(fftw_complex) * cn);

    in_plan = fftw_plan_dft_r2c_2d(in_image->pixel_height, in_image->pixel_width,
				    fftw_in, image_out,
				    FFTW_ESTIMATE);
    filter_plan = fftw_plan_dft_r2c_2d(in_image->pixel_height, in_image->pixel_width,
					fftw_in, filter_out,
					FFTW_ESTIMATE);
    inverse_plan = fftw_plan_dft_c2r_2d(in_image->pixel_height, in_image->pixel_width,
					 image_out, fftw_in,
					 FFTW_ESTIMATE);

    if (copy_alpha)
	num_channels = 3;
    else
	num_channels = 4;
    for (channel = 0; channel < num_channels; ++channel)
    {
	// FFT of input image
	for (i = 0; i < n; ++i)
	    fftw_in[i] = in_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + channel];
	fftw_execute(in_plan);

	// FFT of kernel image
	if (normalize)
	{
	    double d1 = copy_and_add(fftw_in,
				     filter_image->v.floatmap.data + channel + (n - nhalf) * NUM_FLOATMAP_CHANNELS,
				     nhalf);
	    double d2 = copy_and_add(fftw_in + nhalf,
				     filter_image->v.floatmap.data + channel,
				     n - nhalf);
	    double factor = 1.0 / (d1 + d2);

	    for (i = 0; i < n; ++i)
		fftw_in[i] *= factor;
	}
	else
	{
	    copy(fftw_in, filter_image->v.floatmap.data + channel + (n - nhalf) * NUM_FLOATMAP_CHANNELS, nhalf);
	    copy(fftw_in + nhalf, filter_image->v.floatmap.data + channel, n - nhalf);
	}
	fftw_execute(filter_plan);

	// multiply in frequency domain
	for (i = 0; i < cn; ++i)
	    image_out[i] *= filter_out[i];

	// reverse FFT
	fftw_execute(inverse_plan);
	for (i = 0; i < n; ++i)
	    out_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + channel] = fftw_in[i] / n;
    }

    // copy alpha channel
    if (copy_alpha)
	for (i = 0; i < n; ++i)
	    out_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + 3]
		= in_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + 3];

    fftw_destroy_plan(in_plan);
    fftw_destroy_plan(filter_plan);
    fftw_destroy_plan(inverse_plan);

    fftw_free(fftw_in);
    fftw_free(image_out);
    fftw_free(filter_out);

    native_filter_cache_entry_set_image(invocation, cache_entry, out_image);

    return out_image;
}

CALLBACK_SYMBOL
image_t*
native_filter_half_convolve (mathmap_invocation_t *invocation, userval_t *args, mathmap_pools_t *pools)
{
    native_filter_cache_entry_t *cache_entry;
    image_t *in_image = args[0].v.image;
    image_t *filter_image = args[1].v.image;
    gboolean copy_alpha = args[2].v.bool_const != 0.0;
    image_t *out_image;
    double *fftw_in;
    fftw_complex *image_out;
    fftw_plan in_plan, inverse_plan;
    int i, n, nhalf, cn, cw, channel, num_channels;

    cache_entry = invocation_lookup_native_filter_invocation(invocation, args, &native_filter_half_convolve);
    if (cache_entry->image != NULL)
	return cache_entry->image;

    if (in_image->type != IMAGE_FLOATMAP)
	in_image = render_image(invocation, in_image,
				invocation->render_width, invocation->render_height, pools, TRUE);
    if (filter_image->type != IMAGE_FLOATMAP
	|| filter_image->pixel_width != in_image->pixel_width
	|| filter_image->pixel_height != in_image->pixel_height)
	filter_image = render_image(invocation, filter_image,
				    in_image->pixel_width, in_image->pixel_height, pools, TRUE);

    out_image = floatmap_alloc(in_image->pixel_width, in_image->pixel_height, &invocation->pools);

    n = in_image->pixel_height * in_image->pixel_width;
    nhalf = in_image->pixel_width * (in_image->pixel_height / 2) + in_image->pixel_width / 2;
    cw = in_image->pixel_width / 2 + 1;
    cn = in_image->pixel_height * cw;

    fftw_in = fftw_malloc(sizeof(double) * n);
    image_out = fftw_malloc(sizeof(fftw_complex) * cn);

    in_plan = fftw_plan_dft_r2c_2d(in_image->pixel_height, in_image->pixel_width,
				    fftw_in, image_out,
				    FFTW_ESTIMATE);
    inverse_plan = fftw_plan_dft_c2r_2d(in_image->pixel_height, in_image->pixel_width,
					 image_out, fftw_in,
					 FFTW_ESTIMATE);

    if (copy_alpha)
	num_channels = 3;
    else
	num_channels = 4;
    for (channel = 0; channel < num_channels; ++channel)
    {
	// FFT of input image
	for (i = 0; i < n; ++i)
	    fftw_in[i] = in_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + channel];
	fftw_execute(in_plan);

	// multiply in frequency domain
	int x, y;

	for (y = 0; y < in_image->pixel_height; ++y)
	    for (x = 0; x < cw; ++x)
	    {
		int out_idx = x + y * in_image->pixel_width;
		int in_idx = out_idx + nhalf;

		if (in_idx >= n)
		    in_idx -= n;

		image_out[x + y * cw] *= filter_image->v.floatmap.data[in_idx * NUM_FLOATMAP_CHANNELS + channel];
	    }

	// reverse FFT
	fftw_execute(inverse_plan);
	for (i = 0; i < n; ++i)
	    out_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + channel] = fftw_in[i] / n;
    }

    // copy alpha channel
    if (copy_alpha)
	for (i = 0; i < n; ++i)
	    out_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + 3]
		= in_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + 3];

    fftw_destroy_plan(in_plan);
    fftw_destroy_plan(inverse_plan);

    fftw_free(fftw_in);
    fftw_free(image_out);

    native_filter_cache_entry_set_image(invocation, cache_entry, out_image);

    return out_image;
}

CALLBACK_SYMBOL
image_t*
native_filter_visualize_fft (mathmap_invocation_t *invocation, userval_t *args, mathmap_pools_t *pools)
{
    native_filter_cache_entry_t *cache_entry;
    image_t *in_image = args[0].v.image;
    gboolean ignore_alpha = args[1].v.bool_const != 0.0;
    image_t *out_image;
    double *fftw_in;
    fftw_complex *image_out;
    fftw_plan in_plan;
    int i, n, cn, cw, channel, num_channels;
    double sqrtn;

    cache_entry = invocation_lookup_native_filter_invocation(invocation, args, &native_filter_visualize_fft);
    if (cache_entry->image != NULL)
	return cache_entry->image;

    if (in_image->type != IMAGE_FLOATMAP)
	in_image = render_image(invocation, in_image,
				invocation->render_width, invocation->render_height, pools, TRUE);

    out_image = floatmap_alloc(in_image->pixel_width, in_image->pixel_height, &invocation->pools);

    n = in_image->pixel_height * in_image->pixel_width;
    sqrtn = sqrt(n);
    cw = in_image->pixel_width / 2 + 1;
    cn = in_image->pixel_height * cw;

    fftw_in = fftw_malloc(sizeof(double) * n);
    image_out = fftw_malloc(sizeof(fftw_complex) * cn);

    in_plan = fftw_plan_dft_r2c_2d(in_image->pixel_height, in_image->pixel_width,
				    fftw_in, image_out,
				    FFTW_ESTIMATE);

    memset(out_image->v.floatmap.data, 0,
	   sizeof(float) * in_image->pixel_width * in_image->pixel_height * NUM_FLOATMAP_CHANNELS);

    if (ignore_alpha)
	num_channels = 3;
    else
	num_channels = 4;
    for (channel = 0; channel < num_channels; ++channel)
    {
	// FFT of input image
	for (i = 0; i < n; ++i)
	    fftw_in[i] = in_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + channel];
	fftw_execute(in_plan);

	// multiply in frequency domain
	int x, y;

	for (y = 0; y < in_image->pixel_height; ++y)
	{
	    int out_y = y + in_image->pixel_height / 2;

	    if (out_y >= in_image->pixel_height)
		out_y -= in_image->pixel_height;

	    for (x = 0; x < cw; ++x)
	    {
		int out_x1 = cw - 1 - x;
		int out_x2 = x + in_image->pixel_width - cw;
		double val = cabs(image_out[x + y * cw]) / sqrtn;

		out_image->v.floatmap.data[(out_x1 + out_y * in_image->pixel_width) * NUM_FLOATMAP_CHANNELS + channel]
		    = val;
		out_image->v.floatmap.data[(out_x2 + out_y * in_image->pixel_width) * NUM_FLOATMAP_CHANNELS + channel]
		    = val;
	    }
	}
    }

    // set alpha channel
    if (ignore_alpha)
	for (i = 0; i < n; ++i)
	    out_image->v.floatmap.data[i * NUM_FLOATMAP_CHANNELS + 3] = 1.0;

    fftw_destroy_plan(in_plan);

    fftw_free(fftw_in);
    fftw_free(image_out);

    native_filter_cache_entry_set_image(invocation, cache_entry, out_image);

    return out_image;
}
