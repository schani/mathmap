/* -*- c -*- */

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
 * $$2 -> GIMP2 ? 1 : 0
 * $$m -> mathmap code
 * $$p -> USER_CURVE_POINTS
 * $$q -> USER_GRADIENT_POINTS
 * $$o -> OPENSTEP
 * $$a -> has altivec
 */

#include <stdlib.h>
#include <math.h>
#include <complex.h>

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
#if !$o
#if !$2
	    unsigned char button_value[4];
#else
	    struct { double r, g, b, a; } button_value;
#endif
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
#if $o
	    int width;
	    int height;
	    int row_stride;
	    float middle_x;
	    float middle_y;
	    void *data;
#else
	    int index;
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

    int num_uservals;

    int is_native;

    void *initfunc;
    void *module_info;

    void *expression;
    int exprlen;
} mathmap_t;

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

    tuple_t *stack;
    int stackp;

    int exprp;

    int row_stride;
    volatile int num_rows_finished;
    int uses_ra;

    void *mathfunc;
} mathmap_invocation_t;

typedef void (*mathfunc_t) (mathmap_invocation_t*, int, int, unsigned char*);

#if $o
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

complex float cgamma (complex float z);

#define ADD(a,b)              ((a)+(b))
#define SUB(a,b)              ((a)-(b))
#define NEG(a)                (-(a))
#define MUL(a,b)              ((a)*(b))
#define DIV(a,b)              ((float)(a)/(float)(b))
#define MOD(a,b)              (fmod((a),(b)))
#define GAMMA(a)              (((a) > 171.0) ? 0.0 : gsl_sf_gamma((a)))
#define EQ(a,b)               ((a)==(b))
#define LESS(a,b)             ((a)<(b))
#define LEQ(a,b)              ((a)<=(b))
#define NOT(a)                (!(a))
#define PRINT(a)              (printf("%f ", (float)(a)), 0)
#define NEWLINE()             (printf("\n"))
#define COMPLEX(r,i)          ((r) + (i) * I)
#define MAKE_M2X2(a,b,c,d)           ({ gsl_matrix *m = gsl_matrix_alloc(2,2); \
                                        gsl_matrix_set(m,0,0,(a)); gsl_matrix_set(m,0,1,(b)); gsl_matrix_set(m,1,0,(c)); gsl_matrix_set(m,1,1,(d)); m; })
#define MAKE_M3X3(a,b,c,d,e,f,g,h,i) ({ gsl_matrix *m = gsl_matrix_alloc(3,3); \
                                        gsl_matrix_set(m,0,0,(a)); gsl_matrix_set(m,0,1,(b)); gsl_matrix_set(m,0,2,(c)); \
                                        gsl_matrix_set(m,1,0,(d)); gsl_matrix_set(m,1,1,(e)); gsl_matrix_set(m,1,2,(f)); \
                                        gsl_matrix_set(m,2,0,(g)); gsl_matrix_set(m,2,1,(h)); gsl_matrix_set(m,2,2,(i)); m; })
#define FREE_MATRIX(m)        (gsl_matrix_free((m)), 0)
#define MAKE_V2(a,b)          ({ gsl_vector *v = gsl_vector_alloc(2); gsl_vector_set(v,0,(a)); gsl_vector_set(v,1,(b)); v; })
#define MAKE_V3(a,b,c)        ({ gsl_vector *v = gsl_vector_alloc(3); gsl_vector_set(v,0,(a)); gsl_vector_set(v,1,(b)); gsl_vector_set(v,2,(c)); v; })
#define FREE_VECTOR(v)        (gsl_vector_free((v)), 0)
#define VECTOR_NTH(i,v)       gsl_vector_get((v), (i))
#define SOLVE_LINEAR_2(m,v)   ({ gsl_vector *r = gsl_vector_alloc(2); gsl_linalg_HH_solve(m,v,r); r; })
#define SOLVE_LINEAR_3(m,v)   ({ gsl_vector *r = gsl_vector_alloc(3); gsl_linalg_HH_solve(m,v,r); r; })
#define RAND(a,b)             ((rand() / (float)RAND_MAX) * ((b) - (a)) + (a))
#define USERVAL_INT(x)        (invocation->uservals[(x)].v.int_const)
#define USERVAL_FLOAT(x)      (invocation->uservals[(x)].v.float_const)
#define USERVAL_BOOL(x)       (invocation->uservals[(x)].v.bool_const)
#define USERVAL_CURVE(x,p)    (invocation->uservals[(x)].v.curve.values[(int)(MAX(0, MIN(1, (p))) * ($p - 1))])
#define USERVAL_COLOR(x)      (invocation->uservals[(x)].v.color.value)
#define USERVAL_GRADIENT(x,p) (invocation->uservals[(x)].v.gradient.values[(int)(MAX(0, MIN(1, (p))) * ($q - 1))])
#define CLAMP01(x)            (MAX(0,MIN(1,(x))))
#define ORIG_VAL(x,y,d,f)     get_orig_val_pixel_func(invocation, (x), (y), (d), (f))

#if $o
#define RED_FLOAT(c)          (((RED(c)*(ALPHA(c)+1))>>8)/255.0)
#define GREEN_FLOAT(c)        (((GREEN(c)*(ALPHA(c)+1))>>8)/255.0)
#define BLUE_FLOAT(c)         (((BLUE(c)*(ALPHA(c)+1))>>8)/255.0)
#define ALPHA_FLOAT(c)        (ALPHA(c)/255.0)
#define MAKE_COLOR(r,g,b,a)   ({ float _a = CLAMP01((a)); MAKE_RGBA_COLOR(CLAMP01((r))*_a*255,CLAMP01((g))*_a*255,CLAMP01((b))*_a*255,_a*255); })
#define OUTPUT_COLOR(c)       ({ (*(color_t*)p = (c)); 0; })
#else
#define RED_FLOAT(c)          (RED(c)/255.0)
#define GREEN_FLOAT(c)        (GREEN(c)/255.0)
#define BLUE_FLOAT(c)         (BLUE(c)/255.0)
#define ALPHA_FLOAT(c)        (ALPHA(c)/255.0)
#define MAKE_COLOR(r,g,b,a)   (MAKE_RGBA_COLOR(CLAMP01((r))*255,CLAMP01((g))*255,CLAMP01((b))*255,CLAMP01((a))*255))
#define OUTPUT_COLOR(c) \
    ({ if (is_bw) { \
	p[0] = (RED(c)*299 + GREEN(c)*587 + BLUE(c)*114)/1000; \
    } else { \
	p[0] = RED(c); p[1] = GREEN(c); p[2] = BLUE(c); \
    } \
    if (need_alpha) \
	p[alpha_index] = ALPHA(c); \
    0; \
    })
#endif

void
mathmapfunc (mathmap_invocation_t *invocation, int first_row, int last_row, unsigned char *q)
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

    first_row = MAX(0, first_row);
    last_row = MIN(last_row, invocation->img_height);

#if $o
    if (invocation->antialiasing)
	get_orig_val_pixel_func = get_orig_val_intersample_pixel_fast;
    else
	get_orig_val_pixel_func = get_orig_val_pixel_fast;
#else
    if (invocation->antialiasing)
	get_orig_val_pixel_func = get_orig_val_intersample_pixel;
    else
	get_orig_val_pixel_func = get_orig_val_pixel;
#endif

    for (row = first_row; row < last_row; ++row)
    {
	float y = middle_y - (float)(row + origin_y) * scale_y;
	unsigned char *p = q;

	for (col = 0; col < invocation->img_width; ++col)
	{
	    float x = (float)(col + origin_x) * scale_x - middle_x;
	    float r, a;

	    if (invocation->uses_ra)
	    {
		r = hypot(x, y);
		if (r == 0.0)
		    a = 0.0;
		else
		    a = acos(x / r);

		if (y < 0)
		    a = 2 * M_PI - a;
	    }

	    {
$m
	    }

	    p += output_bpp;
	}

	q += invocation->row_stride;

	if (!invocation->supersampling)
	    invocation->num_rows_finished = row + 1;
    }
}

mathfunc_t
mathmapinit (mathmap_invocation_t *invocation)
{
    return mathmapfunc;
}
