/*
 * new_template.c
 *
 * MathMap
 *
 * Copyright (C) 2002-2007 Mark Probst
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * $$g -> GIMP ? 1 : 0
 * $$m -> mathmap code
 * $$p -> USER_CURVE_POINTS
 * $$q -> USER_GRADIENT_POINTS
 * $$xy_decls         -> declarations for xy-constant variables
 * $$xy_code          -> code for xy-constant variables
 * $$x_decls          -> declarations for x-constant variables
 * $$x_code           -> code for x-constant variables
 * $$y_decls          -> declarations for y-constant variables
 * $$y_code           -> code for y-constant variables
 * $$opmacros_h       -> full name of opmacros.h file
 * $$max_debug_tuples -> MAX_DEBUG_TUPLES
 */

#include <stdlib.h>
#include <math.h>
#include <complex.h>

#if !$g
#define OPENSTEP
#endif

#define IN_COMPILED_CODE

#include "$include/opmacros.h"
#include "$include/pools.h"

#ifndef MIN
#define MIN(a,b)         (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b)         (((a)<(b))?(b):(a))
#endif

#define M_PI		3.14159265358979323846	/* pi */

#define EDGE_BEHAVIOUR_COLOR          1
#define EDGE_BEHAVIOUR_WRAP           2
#define EDGE_BEHAVIOUR_REFLECT        3
#define EDGE_BEHAVIOUR_ROTATE	      4

#define USERVAL_IMAGE       7

#define USER_CURVE_POINTS             $p
#define USER_GRADIENT_POINTS          $q

#define MAX_DEBUG_TUPLES              $max_debug_tuples

typedef struct
{
    int number;
    int length;
    float data[];
} tuple_t;

typedef unsigned int color_t;

#define MAKE_RGBA_COLOR(r,g,b,a)            ((((color_t)(r))<<24)|(((color_t)(g))<<16)|(((color_t)(b))<<8)|((color_t)(a)))
#define RED(c)                              ((c)>>24)
#define GREEN(c)                            (((c)>>16)&0xff)
#define BLUE(c)                             (((c)>>8)&0xff)
#define ALPHA(c)                            ((c)&0xff)

#define IMAGE_DRAWABLE		1
#define IMAGE_CLOSURE		2

struct _mathmap_invocation_t;
struct _userval_t;
struct _image_t;

typedef float* (*filter_func_t) (struct _mathmap_invocation_t*,
				 struct _userval_t*,
				 float, float,
				 pools_t*);

typedef struct
{
    float *values;
} curve_t;

typedef struct
{
    color_t *values;
} gradient_t;

typedef struct _userval_t
{
    int type;

    union
    {
	int int_const;
	float float_const;
	float bool_const;
	struct _image_t *image;
	curve_t *curve;
	gradient_t *gradient;

	struct
	{
#if $g
	    struct { double r, g, b, a; } button_value;
#endif
	    color_t value;
	} color;
    } v;

#ifndef OPENSTEP
    void *widget;
#endif
} userval_t;

typedef struct _image_t
{
    int type;
    union {
	void *drawable;
	struct {
	    filter_func_t func;
	    userval_t args[];
	} closure;
    } v;
} image_t;

typedef struct _mathmap_t
{
    void *internals;

    void *filters;
    void *current_filter;
    void *main_filter;

    unsigned int flags;

    void *initfunc;
    void *module_info;

    void *interpreter_insns;
    void *interpreter_values;

    struct _mathmap_t *next;
} mathmap_t;

typedef struct
{
    $xy_decls
} xy_const_vars_t;

typedef struct
{
    $y_decls
} y_const_vars_t;

struct _mathmap_slice_t;

typedef void (*init_frame_func_t) (struct _mathmap_slice_t*);
typedef void (*calc_lines_func_t) (struct _mathmap_slice_t*, int, int, unsigned char*);

typedef struct
{
    init_frame_func_t init_frame;
    calc_lines_func_t calc_lines;
} mathfuncs_t;

typedef struct _mathmap_invocation_t
{
    mathmap_t *mathmap;

    userval_t *uservals;

    int antialiasing;
    int supersampling;

    int output_bpp;

    int edge_behaviour_x, edge_behaviour_y;
    color_t edge_color_x, edge_color_y;

    int current_frame;
    int img_width, img_height;
    float middle_x, middle_y;
    float image_R, image_X, image_Y, image_W, image_H;
    float scale_x, scale_y;

    float current_x, current_y, current_r, current_a, current_t;

    int row_stride;

    unsigned char * volatile rows_finished;

    mathfuncs_t mathfuncs;

    int do_debug;
    int num_debug_tuples;
    tuple_t *debug_tuples[MAX_DEBUG_TUPLES];

    int interpreter_ip;
    color_t interpreter_output_color;
} mathmap_invocation_t;

typedef struct _mathmap_slice_t
{
    mathmap_invocation_t *invocation;

    float sampling_offset_x, sampling_offset_y;
    int region_x, region_y, region_width, region_height;

    xy_const_vars_t *xy_vars;
    y_const_vars_t *y_vars;

    pools_t pools;
} mathmap_slice_t;

extern color_t get_orig_val_pixel (mathmap_invocation_t *invocation, float x, float y, image_t *image, int frame);
extern color_t get_orig_val_intersample_pixel (mathmap_invocation_t *invocation, float x, float y, image_t *image, int frame);

extern float noise (float, float, float);

struct _gsl_vector;
typedef struct _gsl_vector gsl_vector;
struct _gsl_matrix;
typedef struct _gsl_matrix gsl_matrix;

gsl_matrix * gsl_matrix_alloc (const size_t n1, const size_t n2);
void gsl_matrix_free (gsl_matrix * m);
void gsl_matrix_set(gsl_matrix * m, const size_t i, const size_t j, const double x);

gsl_vector *gsl_vector_alloc (const size_t n);
void gsl_vector_free (gsl_vector * v);
double gsl_vector_get (const gsl_vector * v, const size_t i);
void gsl_vector_set (gsl_vector * v, const size_t i, double x);

int gsl_linalg_HH_solve (gsl_matrix * A, const gsl_vector * b, gsl_vector * x);

#define GSL_PREC_SINGLE		1

double gsl_sf_ellint_Kcomp (double k, unsigned int mode);
double gsl_sf_ellint_Ecomp (double k, unsigned int mode);

double gsl_sf_ellint_F (double phi, double k, unsigned int mode);
double gsl_sf_ellint_E (double phi, double k, unsigned int mode);
double gsl_sf_ellint_P (double phi, double k, double n, unsigned int mode);
double gsl_sf_ellint_D (double phi, double k, double n, unsigned int mode);

double gsl_sf_ellint_RC (double x, double y, unsigned int mode);
double gsl_sf_ellint_RD (double x, double y, double z, unsigned int mode);
double gsl_sf_ellint_RF (double x, double y, double z, unsigned int mode);
double gsl_sf_ellint_RJ (double x, double y, double z, double p, unsigned int mode);

int gsl_sf_elljac_e (double u, double m, double *sn, double *cn, double *dn);

complex float cgamma (complex float z);

double gsl_sf_beta (double a, double b);

extern void save_debug_tuples (mathmap_invocation_t *invocation, int row, int col);

$filter_begin
static float*
filter_$name (mathmap_invocation_t *invocation, userval_t *arguments, float x, float y, float t, pools_t *pools);
$filter_end

static void
calc_lines (mathmap_slice_t *slice, int first_row, int last_row, unsigned char *q)
{
    mathmap_invocation_t *invocation = slice->invocation;
    color_t (*get_orig_val_pixel_func) (mathmap_invocation_t*, float, float, image_t*, int);
    int row, col;
    float t = invocation->current_t;
    float X = invocation->image_X, Y = invocation->image_Y;
    float W = invocation->image_W, H = invocation->image_H;
    float R = invocation->image_R;
    float middle_x = invocation->middle_x, middle_y = invocation->middle_y;
    float sampling_offset_x = slice->sampling_offset_x, sampling_offset_y = slice->sampling_offset_y;
    float scale_x = invocation->scale_x, scale_y = invocation->scale_y;
    int origin_x = slice->region_x, origin_y = slice->region_y;
    int frame = invocation->current_frame;
    int output_bpp = invocation->output_bpp;
    int is_bw = output_bpp == 1 || output_bpp == 2;
    int need_alpha = output_bpp == 2 || output_bpp == 4;
    int alpha_index = output_bpp - 1;
    xy_const_vars_t *xy_vars = slice->xy_vars;
    pools_t pixel_pools;
    pools_t *pools;

    init_pools(&pixel_pools);

    first_row = MAX(0, first_row);
    last_row = MIN(last_row, slice->region_y + slice->region_height);

    if (invocation->antialiasing)
	get_orig_val_pixel_func = get_orig_val_intersample_pixel;
    else
	get_orig_val_pixel_func = get_orig_val_pixel;

    for (row = first_row - slice->region_y; row < last_row - slice->region_y; ++row)
    {
	float y = CALC_VIRTUAL_Y(row, origin_y, scale_y, middle_y, sampling_offset_y);
	unsigned char *p = q;

	pools = &slice->pools;

	$x_decls

	$x_code

	pools = &pixel_pools;

	for (col = 0; col < slice->region_width; ++col)
	{
	    y_const_vars_t *y_vars = &slice->y_vars[col];
	    float x = CALC_VIRTUAL_X(col, origin_x, scale_x, middle_x, sampling_offset_x);
	    float *return_tuple;

	    if (invocation->do_debug)
		invocation->num_debug_tuples = 0;

	    reset_pools(pools);

	    {
		$m
	    }

	    if (is_bw)
		p[0] = (TUPLE_RED(return_tuple) * 0.299
			+ TUPLE_GREEN(return_tuple) * 0.587
			+ TUPLE_BLUE(return_tuple) * 0.114) * 255.0;
	    else
	    {
		p[0] = TUPLE_RED(return_tuple) * 255.0;
		p[1] = TUPLE_GREEN(return_tuple) * 255.0;
		p[2] = TUPLE_BLUE(return_tuple) * 255.0;
	    }
	    if (need_alpha)
		p[alpha_index] = TUPLE_ALPHA(return_tuple) * 255.0;

	    if (invocation->do_debug)
		save_debug_tuples(invocation, row, col);

	    p += output_bpp;
	}

	q += invocation->row_stride;

	if (!invocation->supersampling)
	    invocation->rows_finished[row] = 1;
    }

    free_pools(&pixel_pools);
}

static void
init_frame (mathmap_slice_t *slice)
{
    mathmap_invocation_t *invocation = slice->invocation;
    color_t (*get_orig_val_pixel_func) (mathmap_invocation_t*, float, float, image_t*, int);
    float t = invocation->current_t;
    float X = invocation->image_X, Y = invocation->image_Y;
    float W = invocation->image_W, H = invocation->image_H;
    float R = invocation->image_R;
    pools_t *pools = &slice->pools;

    if (invocation->antialiasing)
	get_orig_val_pixel_func = get_orig_val_intersample_pixel;
    else
	get_orig_val_pixel_func = get_orig_val_pixel;

    if (slice->xy_vars != 0)
	free(slice->xy_vars);
    slice->xy_vars = (xy_const_vars_t*)malloc(sizeof(xy_const_vars_t));
    if (slice->y_vars != 0)
	free(slice->y_vars);
    slice->y_vars = (y_const_vars_t*)malloc(sizeof(y_const_vars_t) * slice->region_width);

    {
	xy_const_vars_t *xy_vars = slice->xy_vars;

	{
	    $xy_code
	}
    }

    {
	xy_const_vars_t *xy_vars = slice->xy_vars;
	int col;

	for (col = 0; col < slice->region_width; ++col)
	{
	    y_const_vars_t *y_vars = &slice->y_vars[col];
	    float x = CALC_VIRTUAL_X(col, slice->region_x, invocation->scale_x,
				     invocation->middle_x, slice->sampling_offset_x);

	    {
		$y_code
	    }
	}
    }
}

mathfuncs_t
mathmapinit (mathmap_invocation_t *invocation)
{
    mathfuncs_t funcs;

    funcs.init_frame = &init_frame;
    funcs.calc_lines = &calc_lines;

    return funcs;
}

#undef ARG
#define ARG(i)			(arguments[(i)])

$filter_begin
static float*
filter_$name (mathmap_invocation_t *invocation, userval_t *arguments, float x, float y, float t, pools_t *pools)
{
    color_t (*get_orig_val_pixel_func) (mathmap_invocation_t*, float, float, image_t*, int);
    int frame = 0;
    float X = invocation->image_X, Y = invocation->image_Y;
    float W = invocation->image_W, H = invocation->image_H;
    float R = invocation->image_R;
    float *return_tuple;

#if $g
    if (invocation->antialiasing)
	get_orig_val_pixel_func = get_orig_val_intersample_pixel;
    else
	get_orig_val_pixel_func = get_orig_val_pixel;
#else
    if (invocation->antialiasing)
	get_orig_val_pixel_func = get_orig_val_intersample_pixel_fast;
    else
	get_orig_val_pixel_func = get_orig_val_pixel_fast;
#endif

    $m

    return return_tuple;
}
$filter_end
