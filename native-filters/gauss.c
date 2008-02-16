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
#include "../lispreader/pools.h"

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
gauss_iir (image_t *floatmap, float horizontal_std_dev, float vertical_std_dev, pools_t *pools)
{
    image_t *out;
    int width = floatmap->v.floatmap.width;
    int height = floatmap->v.floatmap.height;
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

    horizontal_std_dev = fabs(horizontal_std_dev * floatmap->v.floatmap.ax);
    vertical_std_dev = fabs(vertical_std_dev * floatmap->v.floatmap.ay);

    horizontal_std_dev = MAX(0.5, horizontal_std_dev);
    vertical_std_dev = MAX(0.5, vertical_std_dev);

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

CALLBACK_SYMBOL
image_t*
native_filter_gaussian_blur (mathmap_invocation_t *invocation, userval_t *args, pools_t *pools)
{
    return gauss_iir(args[0].v.image, args[1].v.float_const, args[2].v.float_const, pools);
}
