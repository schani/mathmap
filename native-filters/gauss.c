/* -*- c -*- */

/*
 * gauss.c
 *
 * MathMap
 *
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 * Copyright (C) 2008 Mark Probst
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

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/param.h>

#include <glib.h>

#include "../drawable.h"
#include "../mmpools.h"

#include "native-filters.h"

static void
find_iir_constants (double *n_p, double *n_m, double *d_p, double *d_m, double *bd_p, double *bd_m, float std_dev)
{
    int i;
    double x0;
    double x1;
    double x2;
    double x3;
    double x4;
    double x5;
    double x6;
    double x7;
    double div;

    /*  The constants used in the implemenation of a casual sequence
     *  using a 4th order approximation of the gaussian operator
     */

    div = sqrt(2 * M_PI) * std_dev;
    x0 = -1.783 / std_dev;
    x1 = -1.723 / std_dev;
    x2 = 0.6318 / std_dev;
    x3 = 1.997  / std_dev;
    x4 = 1.6803 / div;
    x5 = 3.735 / div;
    x6 = -0.6803 / div;
    x7 = -0.2598 / div;

    n_p [0] = x4 + x6;
    n_p [1] = (exp(x1)*(x7*sin(x3)-(x6+2*x4)*cos(x3)) +
	       exp(x0)*(x5*sin(x2) - (2*x6+x4)*cos (x2)));
    n_p [2] = (2 * exp(x0+x1) *
	       ((x4+x6)*cos(x3)*cos(x2) - x5*cos(x3)*sin(x2) -
		x7*cos(x2)*sin(x3)) +
	       x6*exp(2*x0) + x4*exp(2*x1));
    n_p [3] = (exp(x1+2*x0) * (x7*sin(x3) - x6*cos(x3)) +
	       exp(x0+2*x1) * (x5*sin(x2) - x4*cos(x2)));
    n_p [4] = 0.0;

    d_p [0] = 0.0;
    d_p [1] = -2 * exp(x1) * cos(x3) -  2 * exp(x0) * cos (x2);
    d_p [2] = 4 * cos(x3) * cos(x2) * exp(x0 + x1) +  exp(2 * x1) + exp(2 * x0);
    d_p [3] = -2 * cos(x2) * exp(x0 + 2*x1) -  2*cos(x3) * exp(x1 + 2*x0);
    d_p [4] = exp(2*x0 + 2*x1);

    for (i = 0; i <= 4; i++)
	d_m[i] = d_p[i];

    n_m[0] = 0.0;

    for (i = 1; i <= 4; i++)
	n_m[i] = n_p[i] - d_p[i] * n_p[0];

    {
	double sum_n_p, sum_n_m, sum_d;
	double a, b;

	sum_n_p = 0.0;
	sum_n_m = 0.0;
	sum_d = 0.0;

	for (i = 0; i <= 4; i++)
	{
	    sum_n_p += n_p[i];
	    sum_n_m += n_m[i];
	    sum_d += d_p[i];
	}

	a = sum_n_p / (1.0 + sum_d);
	b = sum_n_m / (1.0 + sum_d);

	for (i = 0; i <= 4; i++)
	{
	    bd_p[i] = d_p[i] * a;
	    bd_m[i] = d_m[i] * b;
	}
    }
}

static void
transfer_pixels (const double *src1, const double *src2, float *dest, int width)
{
    int b;

    for (b = 0; b < width; b++)
	*dest++ = *src1++ + *src2++;
}

static image_t*
gauss_iir (image_t *floatmap, float horizontal_std_dev, float vertical_std_dev, mathmap_pools_t *pools)
{
    image_t *out;
    int width = floatmap->pixel_width;
    int height = floatmap->pixel_height;
    float *dest;
    float *src, *sp_p, *sp_m;
    double n_p[5], n_m[5];
    double d_p[5], d_m[5];
    double bd_p[5], bd_m[5];
    double *val_p;
    double *val_m;
    double *vp, *vm;
    int i, j;
    int row, col;
    int terms;
    float initial_p;
    float initial_m;
    int channel;

    out = floatmap_copy(floatmap, pools);

    val_p = g_malloc(MAX(width, height) * sizeof(double));
    val_m = g_malloc(MAX(width, height) * sizeof(double));

    src = g_malloc(MAX(width, height) * sizeof(float));
    dest = g_malloc(MAX(width, height) * sizeof(float));

    /*  First the vertical pass  */
    find_iir_constants (n_p, n_m, d_p, d_m, bd_p, bd_m, vertical_std_dev);

    for (channel = 0; channel < NUM_FLOATMAP_CHANNELS; ++channel)
	for (col = 0; col < width; col++)
	{
	    memset (val_p, 0, height * sizeof (double));
	    memset (val_m, 0, height * sizeof (double));

	    floatmap_get_channel_column(src, out, col, channel);

	    sp_p = src;
	    sp_m = src + (height - 1);
	    vp = val_p;
	    vm = val_m + (height - 1);

	    /*  Set up the first vals  */
	    initial_p = sp_p[0];
	    initial_m = sp_m[0];

	    for (row = 0; row < height; row++)
	    {
		double *vpptr, *vmptr;
		terms = (row < 4) ? row : 4;

		vpptr = vp; vmptr = vm;
		for (i = 0; i <= terms; i++)
		{
		    *vpptr += n_p[i] * sp_p[-i] - d_p[i] * vp[-i];
		    *vmptr += n_m[i] * sp_m[i] - d_m[i] * vm[i];
		}
		for (j = i; j <= 4; j++)
		{
		    *vpptr += (n_p[j] - bd_p[j]) * initial_p;
		    *vmptr += (n_m[j] - bd_m[j]) * initial_m;
		}

		sp_p++;
		sp_m--;
		vp++;
		vm--;
	    }

	    transfer_pixels (val_p, val_m, dest, height);

	    floatmap_set_channel_column(out, col, channel, dest);
	}

    /*  Now the horizontal pass  */
    find_iir_constants (n_p, n_m, d_p, d_m, bd_p, bd_m, horizontal_std_dev);

    for (channel = 0; channel < NUM_FLOATMAP_CHANNELS; ++channel)
	for (row = 0; row < height; row++)
	{
	    memset (val_p, 0, width * sizeof (double));
	    memset (val_m, 0, width * sizeof (double));

	    floatmap_get_channel_row(src, out, row, channel);

	    sp_p = src;
	    sp_m = src + (width - 1);
	    vp = val_p;
	    vm = val_m + (width - 1);

	    /*  Set up the first vals  */
	    initial_p = sp_p[0];
	    initial_m = sp_m[0];

	    for (col = 0; col < width; col++)
	    {
		double *vpptr, *vmptr;

		terms = (col < 4) ? col : 4;

		vpptr = vp;
		vmptr = vm;

		for (i = 0; i <= terms; i++)
		{
		    *vpptr += n_p[i] * sp_p[-i] - d_p[i] * vp[-i];
		    *vmptr += n_m[i] * sp_m[i] - d_m[i] * vm[i];
		}
		for (j = i; j <= 4; j++)
		{
		    *vpptr += (n_p[j] - bd_p[j]) * initial_p;
		    *vmptr += (n_m[j] - bd_m[j]) * initial_m;
		}

		sp_p++;
		sp_m--;
		vp++;
		vm--;
	    }

	    transfer_pixels (val_p, val_m, dest, width);

	    floatmap_set_channel_row(out, row, channel, dest);
	}

    /*  free up buffers  */
    g_free (val_p);
    g_free (val_m);

    g_free (src);
    g_free (dest);

    return out;
}

static void
make_rle_curve (double sigma, float **p_curve, int *p_length, float **p_sum, float *p_total)
{
    const double sigma2 = 2 * sigma * sigma;
    const double l = sqrt (-sigma2 * log (1.0 / 255.0));
    int i, n;
    int length;
    float *sum;
    float *curve;

    n = ceil (l) * 2;
    if ((n % 2) == 0)
	n += 1;

    curve = g_new (float, n);

    length = n / 2;
    curve += length; /* 'center' the curve[] */
    curve[0] = 1.0;

    for (i = 1; i <= length; i++)
    {
	float temp = exp (- (i * i) / sigma2);

	curve[-i] = temp;
	curve[i] = temp;
    }

    sum = g_new (float, 2 * length + 1);

    sum[0] = 0;
    for (i = 1; i <= length*2; i++)
    {
	sum[i] = curve[i-length-1] + sum[i-1];
    }

    sum += length; /* 'center' the sum[] */

    *p_total = sum[length] - sum[-length];
    *p_curve = curve;
    *p_sum = sum;
    *p_length = length;
}

static void
free_rle_curve (float *curve, int length, float *sum)
{
    g_free(sum - length);
    g_free(curve - length);
}

static int
run_length_encode (const float *src, int *rle, float *pix, int dist, int width, int border, gboolean pack)
{
    float last;
    gint count = 0;
    gint i = width;
    gint same = 0;

    src += dist * (width - 1);

    if (pack)
	rle += width + border - 1;

    pix += width + border - 1;

    last  = *src;
    count = 0;

    /* the 'end' border */
    for (i = 0; i < border; i++)
    {
	count++;
	*pix--  = last;

	if (pack)
	    *rle-- = count;
    }

    /* the real pixels */
    for (i = 0; i < width; i++)
    {
	float c = *src;
	src -= dist;

	if (pack && c==last)
        {
	    count++;
	    *pix-- = last;
	    *rle-- = count;
	    same++;
        }
	else
        {
	    count   = 1;
	    last    = c;
	    *pix--  = last;

	    if (pack)
		*rle-- = count;
        }
    }

    /* the start pixels */
    for (i = 0; i < border; i++)
    {
	count++;
	*pix-- = last;

	if (pack)
	    *rle-- = count;
    }

    return same;
}

static void
do_encoded_lre (const gint *enc, const float *src, float *dest, int width, int length, int dist,
		const float *curve, int ctotal, const float *csum)
{
    int col;

    for (col = 0; col < width; col++, dest += dist)
    {
	const int *rpt;
	const float *pix;
	int nb;
	float s1;
	int i;
	float val = 0.0;
	int start = - length;

	rpt = &enc[col + start];
	pix = &src[col + start];

	s1 = csum[start];
	nb = rpt[0];
	i  = start + nb;

	while (i <= length)
        {
	    int s2 = csum[i];

	    val += pix[0] * (s2-s1);
	    s1 = s2;
	    rpt = &rpt[nb];
	    pix = &pix[nb];
	    nb = rpt[0];
	    i += nb;
        }

	val += pix[0] * (csum[length] - s1);

	val = val / ctotal;
	*dest = val;
    }
}

static void
do_full_lre (const float *src, float *dest, int width, int length, int dist, const float *curve, float ctotal)
{
    int col;

    for (col = 0; col < width; col++, dest += dist)
    {
	const float *x1;
	const float *x2;
	const float *c = &curve[0];
	int i;
	float val = 0.0;

	x1 = x2 = &src[col];

	/* The central point is a special case since it should only be
	 * processed ONCE
	 */

	val += x1[0] * c[0];

	c  += 1;
	x1 += 1;
	x2 -= 1;
	i = length;

	/* Processing multiple points in a single iteration should be
	 * faster but is not strictly required.
	 * Some precise benchmarking will be needed to figure out
	 * if this is really interesting.
	 */
	while (i >= 8)
        {
	    val += (x1[0] + x2[-0]) * c[0];
	    val += (x1[1] + x2[-1]) * c[1];
	    val += (x1[2] + x2[-2]) * c[2];
	    val += (x1[3] + x2[-3]) * c[3];
	    val += (x1[4] + x2[-4]) * c[4];
	    val += (x1[5] + x2[-5]) * c[5];
	    val += (x1[6] + x2[-6]) * c[6];
	    val += (x1[7] + x2[-7]) * c[7];

	    c  += 8;
	    x1 += 8;
	    x2 -= 8;
	    i  -= 8;
        }

	while (i >= 4)
        {
	    val += (x1[0] + x2[-0]) * c[0];
	    val += (x1[1] + x2[-1]) * c[1];
	    val += (x1[2] + x2[-2]) * c[2];
	    val += (x1[3] + x2[-3]) * c[3];
	    c  += 4;
	    x1 += 4;
	    x2 -= 4;
	    i  -= 4;
        }

	/* Only that final loop is strictly required */
	while (i >= 1)
        {
	    /* process the pixels at the distance i before and after the
	     * central point. They must have the same coefficient
	     */
	    val += (x1[0] + x2[-0]) * c[0];
	    c  += 1;
	    x1 += 1;
	    x2 -= 1;
	    i  -= 1;
        }

	val = val / ctotal;
	*dest = val;
    }
}

static image_t*
gauss_rle (image_t *floatmap, float horizontal_std_dev, float vertical_std_dev, mathmap_pools_t *pools)
{
    int width = floatmap->pixel_width;
    int height = floatmap->pixel_height;
    image_t *out;
    float *dest;
    float *src;
    int row, col, b;
    float total;
    float *curve;
    float *sum;
    int length;

    src  = g_new(float, MAX(width, height) * NUM_FLOATMAP_CHANNELS);
    dest = g_new(float, MAX(width, height) * NUM_FLOATMAP_CHANNELS);

    out = floatmap_copy(floatmap, pools);

    /*  First the vertical pass  */
    if (vertical_std_dev > 0.0)
    {
	int *rle = NULL;
	float *pix = NULL;

	make_rle_curve (vertical_std_dev, &curve, &length, &sum, &total);

	rle = g_new (int, height + 2 * length);
	rle += length; /* rle[] extends from -length to height+length-1 */

	pix = g_new (float, height + 2 * length);
	pix += length; /* pix[] extends from -length to height+length-1 */

	for (col = 0; col < width; col++)
        {
	    floatmap_get_column(src, out, col);

	    /*
	    if (has_alpha)
		multiply_alpha (src, height, NUM_FLOATMAP_CHANNELS);
	    */

	    for (b = 0; b < NUM_FLOATMAP_CHANNELS; b++)
            {
		int same = run_length_encode (src + b, rle, pix, NUM_FLOATMAP_CHANNELS,
					      height, length, TRUE);

		if (same > (3 * height) / 4)
		{
		    /* encoded_rle is only fastest if there are a lot of
		     * repeating pixels
		     */
		    do_encoded_lre (rle, pix, dest + b, height, length, NUM_FLOATMAP_CHANNELS,
				    curve, total, sum);
		}
		else
		{
		    /* else a full but more simple algorithm is better */
		    do_full_lre (pix, dest + b, height, length, NUM_FLOATMAP_CHANNELS,
				 curve, total);
		}
            }

	    /*
	    if (has_alpha)
		separate_alpha (dest, height, NUM_FLOATMAP_CHANNELS);
	    */

	    floatmap_set_column(out, col, dest);
        }

	g_free(rle - length);
	g_free(pix - length);

	free_rle_curve(curve, length, sum);
    }

    /*  Now the horizontal pass  */
    if (horizontal_std_dev > 0.0)
    {
	int *rle = NULL;
	float *pix = NULL;

	make_rle_curve(horizontal_std_dev, &curve, &length, &sum, &total);

	rle = g_new (int, width+2*length);
	rle += length; /* so rle[] extends from -width to width+length-1 */

	pix = g_new (float, width+2*length);
	pix += length; /* so pix[] extends from -width to width+length-1 */

	for (row = 0; row < height; row++)
        {
	    floatmap_get_row(src, out, row);

	    /*
	    if (has_alpha)
		multiply_alpha (src, width, NUM_FLOATMAP_CHANNELS);
	    */

	    for (b = 0; b < NUM_FLOATMAP_CHANNELS; b++)
            {
		int same = run_length_encode (src + b, rle, pix, NUM_FLOATMAP_CHANNELS,
					      width, length, TRUE);

		if (same > (3 * width) / 4)
		{
		    /* encoded_rle is only fastest if there are a lot of
		     * repeating pixels
		     */
		    do_encoded_lre (rle, pix, dest + b, width, length, NUM_FLOATMAP_CHANNELS,
				    curve, total, sum);
		}
		else
		{
		    /* else a full but more simple algorithm is better */
		    do_full_lre (pix, dest + b, width, length, NUM_FLOATMAP_CHANNELS,
				 curve, total);
		}
            }

	    /*
	    if (has_alpha)
		separate_alpha (dest, width, NUM_FLOATMAP_CHANNELS);
	    */

	    floatmap_set_row(out, row, dest);
        }

	g_free (rle - length);
	g_free (pix - length);

	free_rle_curve (curve, length, sum);
    }

    g_free(src);
    g_free(dest);

    return out;
}

CALLBACK_SYMBOL
image_t*
native_filter_gaussian_blur (mathmap_invocation_t *invocation, userval_t *args, mathmap_pools_t *pools)
{
    image_t *floatmap = args[0].v.image;
    float horizontal_std_dev = args[1].v.float_const;
    float vertical_std_dev = args[2].v.float_const;

    if (floatmap->type != IMAGE_FLOATMAP)
	floatmap = render_image(invocation, floatmap,
				invocation->render_width, invocation->render_height, pools, FALSE);

    horizontal_std_dev = fabs(horizontal_std_dev * floatmap->v.floatmap.ax);
    vertical_std_dev = fabs(vertical_std_dev * floatmap->v.floatmap.ay);

    if (horizontal_std_dev < 0.5 || vertical_std_dev < 0.5)
	return gauss_rle(floatmap, horizontal_std_dev, vertical_std_dev, pools);
    else
	return gauss_iir(floatmap, horizontal_std_dev, vertical_std_dev, pools);
}
