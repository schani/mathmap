/* -*- c -*- */

/*
 * new_template.c
 *
 * MathMap
 *
 * Copyright (C) 2002 Mark Probst
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
 * $$o -> OPENSTEP
 */

#include <stdlib.h>
#include <math.h>

#ifndef MIN
#define MIN(a,b)         (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b)         (((a)<(b))?(b):(a))
#endif

#define M_PI		3.14159265358979323846	/* pi */

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
	    unsigned char button_value[4];
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
    unsigned char edge_color[4];

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

extern void get_orig_val_pixel (mathmap_invocation_t *invocation, float x, float y, unsigned char *pixel, int drawable_index, int frame);
extern void get_orig_val_intersample_pixel (mathmap_invocation_t *invocation, float x, float y, unsigned char *pixel, int drawable_index, int frame);
extern void convert_rgb_to_hsv (float *rgb, float *hsv);
extern void convert_hsv_to_rgb (float *hsv, float *rgb);
extern float noise (float, float, float);

typedef struct
{
    double dat[2];
} gsl_complex;

gsl_complex gsl_complex_div (gsl_complex a, gsl_complex b);  /* r=a/b */
gsl_complex gsl_complex_sqrt (gsl_complex z);  /* r=sqrt(z) */
gsl_complex gsl_complex_pow (gsl_complex a, gsl_complex b);  /* r=a^b */
gsl_complex gsl_complex_exp (gsl_complex a);    /* r=exp(a) */
gsl_complex gsl_complex_log (gsl_complex a);    /* r=log(a) (base e) */
gsl_complex gsl_complex_sin (gsl_complex a);  /* r=sin(a) */
gsl_complex gsl_complex_cos (gsl_complex a);  /* r=cos(a) */
gsl_complex gsl_complex_tan (gsl_complex a);  /* r=tan(a) */
gsl_complex gsl_complex_arcsin (gsl_complex a);  /* r=arcsin(a) */
gsl_complex gsl_complex_arccos (gsl_complex a);  /* r=arccos(a) */
gsl_complex gsl_complex_arctan (gsl_complex a);  /* r=arctan(a) */
gsl_complex gsl_complex_sinh (gsl_complex a);  /* r=sinh(a) */
gsl_complex gsl_complex_cosh (gsl_complex a);  /* r=coshh(a) */
gsl_complex gsl_complex_tanh (gsl_complex a);  /* r=tanh(a) */
gsl_complex gsl_complex_arcsinh (gsl_complex a);  /* r=arcsinh(a) */
gsl_complex gsl_complex_arccosh (gsl_complex a);  /* r=arccosh(a) */
gsl_complex gsl_complex_arctanh (gsl_complex a);  /* r=arctanh(a) */

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

gsl_complex cgamma (gsl_complex z);

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
#define ORIG_VAL(x,y,d,f)     ({ unsigned char p[4]; get_orig_val_pixel_func(invocation, (x), (y), p, (d), (f)); MAKE_RGBA_COLOR(p[0], p[1], p[2], p[3]); })
#define COMPLEX(r,i)          ((gsl_complex){{(r), (i)}})
#define C_REAL(z)             ((z).dat[0])
#define C_IMAG(z)             ((z).dat[1])
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
    void (*get_orig_val_pixel_func) (mathmap_invocation_t*, float, float, unsigned char*, int, int);
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

    if (invocation->antialiasing)
	get_orig_val_pixel_func = get_orig_val_intersample_pixel;
    else
	get_orig_val_pixel_func = get_orig_val_pixel;

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

	invocation->num_rows_finished = row + 1;
    }
}

mathfunc_t
mathmapinit (mathmap_invocation_t *invocation)
{
    return mathmapfunc;
}
