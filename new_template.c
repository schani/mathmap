/*
 * new_template.c
 *
 * MathMap
 *
 * Copyright (C) 2002-2004 Mark Probst
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
 * $$l -> MAX_TUPLE_LENGTH
 * $$g -> GIMP ? 1 : 0
 * $$m -> mathmap code
 * $$p -> USER_CURVE_POINTS
 * $$q -> USER_GRADIENT_POINTS
 * $$a -> has altivec
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

#include "$opmacros_h"

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

#define USERVAL_IMAGE       7

#define USER_CURVE_POINTS             $p
#define USER_GRADIENT_POINTS          $q

#define MAX_DEBUG_TUPLES              $max_debug_tuples

typedef struct
{
    float data[$l];
    int number;
    int length;
} tuple_t;

typedef unsigned int color_t;

#define MAKE_RGBA_COLOR(r,g,b,a)            ((((color_t)(r))<<24)|(((color_t)(g))<<16)|(((color_t)(b))<<8)|((color_t)(a)))
#define RED(c)                              ((c)>>24)
#define GREEN(c)                            (((c)>>16)&0xff)
#define BLUE(c)                             (((c)>>8)&0xff)
#define ALPHA(c)                            ((c)&0xff)

typedef struct _userval_t
{
    int type;

    union
    {
	int int_const;
	float float_const;
	float bool_const;

	struct
	{
#if $g
	    struct { double r, g, b, a; } button_value;
#endif
	    color_t value;
	} color;

	struct
	{
	    float *values;
	} curve;

	struct
	{
	    color_t *values;
	} gradient;

	struct
	{
#if $g
	    int index;
#else
	    int width;
	    int height;
	    int row_stride;
	    float middle_x;
	    float middle_y;
	    void *data;
#endif
	} image;
    } v;

#if $g
    void *widget;
#endif
} userval_t;

typedef struct _mathmap_t
{
    void *userval_infos;
    void *variables;
    void *internals;

    void *exprtree;
    /* void *top_level_decls; */

    int num_uservals;

    int is_native;

    void *initfunc;
    void *module_info;

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

struct _mathmap_invocation_t;

typedef void (*init_frame_func_t) (struct _mathmap_invocation_t*);
typedef void (*calc_lines_func_t) (struct _mathmap_invocation_t*, int, int, unsigned char*);

typedef struct
{
    init_frame_func_t init_frame;
    calc_lines_func_t calc_lines;
} mathfuncs_t;

typedef struct _mathmap_invocation_t
{
    mathmap_t *mathmap;

    userval_t *uservals;
    tuple_t *variables;
    tuple_t *internals;

    int antialiasing;
    int supersampling;

    int output_bpp;

    int edge_behaviour;
    color_t edge_color;

    int current_frame;
    int origin_x, origin_y;
    int img_width, img_height;
    float middle_x, middle_y;
    float image_R, image_X, image_Y, image_W, image_H;
    float scale_x, scale_y;

    float current_x, current_y, current_r, current_a, current_t;

    int row_stride;
    volatile int num_rows_finished;

    mathfuncs_t mathfuncs;

    xy_const_vars_t *xy_vars;
    y_const_vars_t *y_vars;

    int cmdline;

    int do_debug;
    int num_debug_tuples;
    tuple_t debug_tuples[MAX_DEBUG_TUPLES];
} mathmap_invocation_t;

#if !$g
static color_t
get_orig_val_pixel_fast (mathmap_invocation_t *invocation, float _x, float _y, int drawable_index, int frame)
{
    int x, y;
    int width, height;
    userval_t *userval;

    if (drawable_index < 0 || drawable_index >= invocation->mathmap->num_uservals
	|| invocation->uservals[drawable_index].type != USERVAL_IMAGE)
	return MAKE_RGBA_COLOR(255,255,255,255); /* illegal image */

    userval = &invocation->uservals[drawable_index];

    x = floor(_x + userval->v.image.middle_x);
    y = floor(-_y + userval->v.image.middle_y);

    width = userval->v.image.width;
    height = userval->v.image.height;
    
    if (invocation->edge_behaviour == EDGE_BEHAVIOUR_WRAP)
    {
	if (x < 0)
	    x = x % width + width;
	else if (x >= width)
	    x %= width;
	if (y < 0)
	    y = y % height + height;
	else if (y >= height)
	    y %= height;
    }
    else if (invocation->edge_behaviour == EDGE_BEHAVIOUR_REFLECT)
    {
	if (x < 0)
	    x = -x % width;
	else if (x >= width)
	    x = (width - 1) - (x % width);
	if (y < 0)
	    y = -y % height;
	else if (y >= height)
	    y = (height - 1) - (y % height);
    }
    else
    {
	if (x < 0 || x >= width || y < 0 || y >= height)
	    return *(color_t*)invocation->edge_color;
    }

    return *(color_t*)(userval->v.image.data + 4 * x + y * userval->v.image.row_stride);
}

extern color_t get_orig_val_intersample_pixel (mathmap_invocation_t *invocation, float x, float y, int drawable_index, int frame);

#if $a
typedef union
{
  unsigned int a[4];
  vector unsigned int v;
} uint_vector;

typedef union
{
  float a[4];
  vector float v;
} float_vector;

static color_t
get_orig_val_intersample_pixel_fast (mathmap_invocation_t *invocation, float x, float y, int drawable_index, int frame)
{
    int width, height;
    userval_t *userval;
    float cx, cy;
    color_t *img_data;
    int row_stride;

    if (drawable_index < 0 || drawable_index >= invocation->mathmap->num_uservals
	|| invocation->uservals[drawable_index].type != USERVAL_IMAGE)
	return MAKE_RGBA_COLOR(255,255,255,255); /* illegal image */

    userval = &invocation->uservals[drawable_index];

    cx = x + userval->v.image.middle_x;
    cy = -y + userval->v.image.middle_y;

    width = userval->v.image.width;
    height = userval->v.image.height;

    if (cx < 0 || cx >= width - 1
	|| cy < 0 || cy >= height - 1)
	return get_orig_val_intersample_pixel(invocation, x, y, drawable_index, frame);

    img_data = userval->v.image.data;
    row_stride = userval->v.image.row_stride >> 2; /* assuming row_stride is multiple of 4 */

    {
	vector float one_one = (vector float)(1.0, 1.0, 0.0, 0.0);
	vector unsigned char pv_x0x0x1x1 =
	    (vector unsigned char)(0,1,2,3,0,1,2,3,16,17,18,19,16,17,18,19);
	vector unsigned char pv_y0y1y0y1 =
	    (vector unsigned char)(4,5,6,7,20,21,22,23,4,5,6,7,20,21,22,24);
	vector unsigned int zero = (vector unsigned int)(0,0,0,0);
	vector unsigned char c_mask =
	    (vector unsigned char)(0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255);

	color_t __attribute__((aligned(16))) color;
	float_vector cxy_u;
	vector float cxy, cxy0, cxyd0, cxyd1;
	vector unsigned int pxy0;
	uint_vector pxy0_u;
	vector float cxd0xd0xd1xd1, cyd0yd1yd0yd1;
	vector float fs;
	unsigned int i00;
	uint_vector cs_u;
	vector signed char cs;
	vector signed short cs_h;
	vector float c00, c01, c10, c11;
	vector float c;
	vector unsigned int ci;

	cxy_u.a[0] = cx;
	cxy_u.a[1] = cy;

	cxy = cxy_u.v;

	cxy0 = vec_floor(cxy);
	cxyd1 = vec_sub(cxy, cxy0);
	cxyd0 = vec_sub(one_one, cxyd1);

	pxy0 = vec_ctu(cxy0, 0);
	pxy0_u.v = pxy0;

	cxd0xd0xd1xd1 = vec_perm(cxyd0, cxyd1, pv_x0x0x1x1);
	cyd0yd1yd0yd1 = vec_perm(cxyd0, cxyd1, pv_y0y1y0y1);

	fs = vec_madd(cxd0xd0xd1xd1, cyd0yd1yd0yd1, (vector float)zero);

	i00 = pxy0_u.a[0] + pxy0_u.a[1] * row_stride;

	cs_u.a[0] = img_data[i00];
	cs_u.a[1] = img_data[i00 + 1];
	cs_u.a[2] = img_data[i00 + row_stride];
	cs_u.a[3] = img_data[i00 + row_stride + 1];

	cs = (vector signed char)cs_u.v;

	cs_h = vec_unpackh(cs);
	c00 = vec_ctf(vec_and(vec_unpackh(cs_h),
			      (vector signed int)c_mask), 0);
	c10 = vec_ctf(vec_and(vec_unpackl(cs_h),
			      (vector signed int)c_mask), 0);

	cs_h = vec_unpackl(cs);
	c01 = vec_ctf(vec_and(vec_unpackh(cs_h),
			      (vector signed int)c_mask), 0);
	c11 = vec_ctf(vec_and(vec_unpackl(cs_h),
			      (vector signed int)c_mask), 0);

	c = vec_madd(c00, vec_splat(fs, 0), (vector float)zero);
	c = vec_madd(c01, vec_splat(fs, 1), c);
	c = vec_madd(c10, vec_splat(fs, 2), c);
	c = vec_madd(c11, vec_splat(fs, 3), c);

	ci = (vector unsigned int)vec_cts(c, 0);

	ci = (vector unsigned int)vec_packs(vec_packs(ci, zero),
					    (vector unsigned short)zero);
	      
	vec_ste(ci, 0, &color);

	return color;
    }
	
}
#else
static color_t
get_orig_val_intersample_pixel_fast (mathmap_invocation_t *invocation, float x, float y, int drawable_index, int frame)
{
    return get_orig_val_intersample_pixel(invocation, x, y, drawable_index, frame);
}
#endif
#else
extern color_t get_orig_val_pixel (mathmap_invocation_t *invocation, float x, float y, int drawable_index, int frame);
extern color_t get_orig_val_intersample_pixel (mathmap_invocation_t *invocation, float x, float y, int drawable_index, int frame);
#endif

extern float noise (float, float, float);

struct _gsl_vector;
typedef struct _gsl_vector gsl_vector;
struct _gsl_matrix;
typedef struct _gsl_matrix gsl_matrix;

gsl_matrix * gsl_matrix_alloc (const size_t n1, const size_t n2);
void gsl_matrix_free (gsl_matrix * m);
void    gsl_matrix_set(gsl_matrix * m, const size_t i, const size_t j, const double x);

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

extern void save_debug_tuples (mathmap_invocation_t *invocation, int row, int col);

static void
calc_lines (mathmap_invocation_t *invocation, int first_row, int last_row, unsigned char *q)
{
    color_t (*get_orig_val_pixel_func) (mathmap_invocation_t*, float, float, int, int);
    int row, col;
    float t = invocation->current_t;
    float X = invocation->image_X, Y = invocation->image_Y;
    float W = invocation->image_W, H = invocation->image_H;
    float R = invocation->image_R;
    float middle_x = invocation->middle_x, middle_y = invocation->middle_y;
    float scale_x = invocation->scale_x, scale_y = invocation->scale_y;
    int origin_x = invocation->origin_x, origin_y = invocation->origin_y;
    int frame = invocation->current_frame;
    int output_bpp = invocation->output_bpp;
    int is_bw = output_bpp == 1 || output_bpp == 2;
    int need_alpha = output_bpp == 2 || output_bpp == 4;
    int alpha_index = output_bpp - 1;
    xy_const_vars_t *xy_vars = invocation->xy_vars;

    first_row = MAX(0, first_row);
    last_row = MIN(last_row, invocation->img_height);

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

    for (row = first_row; row < last_row; ++row)
    {
	float y = middle_y - (float)(row + origin_y) * scale_y;
	unsigned char *p = q;

	$x_decls

	$x_code

	for (col = 0; col < invocation->img_width; ++col)
	{
	    y_const_vars_t *y_vars = &invocation->y_vars[col];
	    float x = (float)(col + origin_x) * scale_x - middle_x;

#if $uses_ra
	    float r, a;

	    r = hypot(x, y);
	    if (r == 0.0)
		a = 0.0;
	    else
		a = acos(x / r);

	    if (y < 0)
		a = 2 * M_PI - a;
#endif

	    if (invocation->do_debug)
		invocation->num_debug_tuples = 0;

	    {
		$m
	    }

	    if (invocation->do_debug)
		save_debug_tuples(invocation, row, col);

	    p += output_bpp;
	}

	q += invocation->row_stride;

	if (!invocation->supersampling)
	    invocation->num_rows_finished = row + 1;
    }
}

static void
init_frame (mathmap_invocation_t *invocation)
{
    color_t (*get_orig_val_pixel_func) (mathmap_invocation_t*, float, float, int, int);
    float t = invocation->current_t;
    float X = invocation->image_X, Y = invocation->image_Y;
    float W = invocation->image_W, H = invocation->image_H;
    float R = invocation->image_R;

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

    if (invocation->xy_vars != 0)
	free(invocation->xy_vars);
    invocation->xy_vars = (xy_const_vars_t*)malloc(sizeof(xy_const_vars_t));
    if (invocation->y_vars != 0)
	free(invocation->y_vars);
    invocation->y_vars = (y_const_vars_t*)malloc(sizeof(y_const_vars_t) * invocation->img_width);

    {
	xy_const_vars_t *xy_vars = invocation->xy_vars;

	{
	    $xy_code
	}
    }

    {
	xy_const_vars_t *xy_vars = invocation->xy_vars;
	int col;

	for (col = 0; col < invocation->img_width; ++col)
	{
	    y_const_vars_t *y_vars = &invocation->y_vars[col];
	    float x = (float)(col + invocation->origin_x) * invocation->scale_x - invocation->middle_x;

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
