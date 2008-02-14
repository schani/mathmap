/* -*- c -*- */

/*
 * opmacros.h
 *
 * MathMap
 *
 * Copyright (C) 2004-2007 Mark Probst
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

#ifndef __OPMACROS_H__
#define __OPMACROS_H__

typedef struct
{
    float v[2];
} mm_v2_t;

typedef struct
{
    float v[3];
} mm_v3_t;

typedef struct
{
    float a00;
    float a01;
    float a10;
    float a11;
} mm_m2x2_t;

#define NOP()                 (0.0)

#define INT2FLOAT(x)          ((float)(x))
#define FLOAT2INT(x)	      ((int)(x))
#define INT2COMPLEX(x)        ((complex float)(x))
#define FLOAT2COMPLEX(x)      ((complex float)(x))

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

#define PRINT_FLOAT(a)        (printf("%f ", (float)(a)), 0)
#define NEWLINE()             (printf("\n"))

#define START_DEBUG_TUPLE(n)       ({ if (invocation->do_debug && invocation->num_debug_tuples < MAX_DEBUG_TUPLES) { \
                                          invocation->debug_tuples[invocation->num_debug_tuples]->length = 0; \
                                          invocation->debug_tuples[invocation->num_debug_tuples]->number = (n); \
                                          ++invocation->num_debug_tuples; } \
                                      0; })
/* this assumes that operator calls are not reordered */
#define SET_DEBUG_TUPLE_DATA(i,v)  ({ if (invocation->do_debug && invocation->num_debug_tuples < MAX_DEBUG_TUPLES) { \
                                          invocation->debug_tuples[invocation->num_debug_tuples - 1]->data[(int)(i)] = (v); \
                                          invocation->debug_tuples[invocation->num_debug_tuples - 1]->length = (i) + 1; } \
                                      0; })

#define COMPLEX(r,i)          ((r) + (i) * I)

// matrices
#define MAKE_M2X2(a,b,c,d)	     ({ mm_m2x2_t m = { (a), (b), (c), (d) }; m; })
#define MAKE_GSL_M2X2(m)             ({ mm_m2x2_t mm = (m); gsl_matrix *gm = gsl_matrix_alloc(2,2); \
                                        gsl_matrix_set(gm,0,0,mm.a00); gsl_matrix_set(gm,0,1,mm.a01); \
					gsl_matrix_set(gm,1,0,mm.a10); gsl_matrix_set(gm,1,1,mm.a11); gm; })
#define MAKE_M3X3(a,b,c,d,e,f,g,h,i) ({ gsl_matrix *m = gsl_matrix_alloc(3,3); \
                                        gsl_matrix_set(m,0,0,(a)); gsl_matrix_set(m,0,1,(b)); gsl_matrix_set(m,0,2,(c)); \
                                        gsl_matrix_set(m,1,0,(d)); gsl_matrix_set(m,1,1,(e)); gsl_matrix_set(m,1,2,(f)); \
                                        gsl_matrix_set(m,2,0,(g)); gsl_matrix_set(m,2,1,(h)); gsl_matrix_set(m,2,2,(i)); m; })
#define FREE_MATRIX(m)        (gsl_matrix_free((m)), 0)

// vectors
#define MAKE_V2(a,b)	      ({ mm_v2_t v = { { (a), (b) } }; v; })
#define MAKE_V3(a,b,c)	      ({ mm_v3_t v = { { (a), (b), (c) } }; v; })

#define MAKE_GSL_V2(_mv)      ({ mm_v2_t mv = (_mv); gsl_vector *gv = gsl_vector_alloc(2); \
				 gsl_vector_set(gv,0,mv.v[0]); gsl_vector_set(gv,1,mv.v[1]); gv; })
#define MAKE_GSL_V3(_mv)      ({ mm_v3_t mv = (_mv); gsl_vector *gv = gsl_vector_alloc(3); \
				 gsl_vector_set(gv,0,mv.v[0]); gsl_vector_set(gv,1,mv.v[1]); gsl_vector_set(gv,2,mv.v[2]); gv; })
#define FREE_GSL_VECTOR(gv)   (gsl_vector_free((gv)), 0)
#define VECTOR_NTH(i,vec)     ((vec).v[(int)(i)])

// solvers
#define SOLVE_LINEAR_2(mm,mv) ({ gsl_vector *gv = MAKE_GSL_V2((mv)); gsl_matrix *gm = MAKE_GSL_M2X2((mm)); \
	    			 gsl_vector *gr = gsl_vector_alloc(2); gsl_linalg_HH_solve(gm,gv,gr); \
				 mm_v2_t r = { { gsl_vector_get(gr, 0), gsl_vector_get(gr, 1) } }; \
				 FREE_GSL_VECTOR(gv); FREE_GSL_VECTOR(gr); FREE_MATRIX(gm); r; })
#define SOLVE_LINEAR_3(gm,mv) ({ gsl_vector *gv = MAKE_GSL_V3((mv)); \
	    			 gsl_vector *gr = gsl_vector_alloc(3); gsl_linalg_HH_solve(gm,gv,gr); \
				 mm_v3_t r = { { gsl_vector_get(gr, 0), gsl_vector_get(gr, 1), gsl_vector_get(gr, 2) } }; \
				 FREE_GSL_VECTOR(gv); FREE_GSL_VECTOR(gr); r; })
/* FIXME: implement these! */
#define SOLVE_POLY_2(a,b,c)   ({ mm_v2_t v = { { 0.0, 0.0 } }; v; })
#define SOLVE_POLY_3(a,b,c,d) ({ mm_v3_t v = { { 0.0, 0.0, 0.0 } }; v; })

// elliptics
#define ELL_INT_K_COMP(k)     gsl_sf_ellint_Kcomp((k), GSL_PREC_SINGLE)
#define ELL_INT_E_COMP(k)     gsl_sf_ellint_Ecomp((k), GSL_PREC_SINGLE)

#define ELL_INT_F(phi,k)      gsl_sf_ellint_F((phi), (k), GSL_PREC_SINGLE)
#define ELL_INT_E(phi,k)      gsl_sf_ellint_E((phi), (k), GSL_PREC_SINGLE)
#define ELL_INT_P(phi,k,n)    gsl_sf_ellint_P((phi), (k), (n), GSL_PREC_SINGLE)
#define ELL_INT_D(phi,k,n)    gsl_sf_ellint_D((phi), (k), (n), GSL_PREC_SINGLE)

#define ELL_INT_RC(x,y)       gsl_sf_ellint_RC((x), (y), GSL_PREC_SINGLE)
#define ELL_INT_RD(x,y,z)     gsl_sf_ellint_RD((x), (y), (z), GSL_PREC_SINGLE)
#define ELL_INT_RF(x,y,z)     gsl_sf_ellint_RF((x), (y), (z), GSL_PREC_SINGLE)
#define ELL_INT_RJ(x,y,z,p)   gsl_sf_ellint_RJ((x), (y), (z), (p), GSL_PREC_SINGLE)

#define ELL_JAC(u,m)	      ({ double sn, cn, dn; \
				 gsl_sf_elljac_e((u), (m), &sn, &cn, &dn); \
				 mm_v3_t v = { { sn, cn, dn } }; v; })

#define RAND(a,b)             ((rand() / (float)RAND_MAX) * ((b) - (a)) + (a))
#define CLAMP01(x)            (MAX(0,MIN(1,(x))))

#define ARG(i)	(invocation->uservals[(i)])

#define USERVAL_INT_ACCESS(x)        (ARG((x)).v.int_const)
#define USERVAL_FLOAT_ACCESS(x)      (ARG((x)).v.float_const)
#define USERVAL_BOOL_ACCESS(x)       (ARG((x)).v.bool_const)
#define USERVAL_COLOR_ACCESS(x)      (ARG((x)).v.color.value)
#define USERVAL_CURVE_ACCESS(x)	     (ARG((x)).v.curve)
#define USERVAL_GRADIENT_ACCESS(x)   (ARG((x)).v.gradient)
#define USERVAL_IMAGE_ACCESS(x)      (ARG((x)).v.image)

#define UNINITED_IMAGE        (0)

#ifdef IN_COMPILED_CODE
#ifdef OPENSTEP
#define RED_FLOAT(c)          (((RED(c)*(ALPHA(c)+1))>>8)/255.0)
#define GREEN_FLOAT(c)        (((GREEN(c)*(ALPHA(c)+1))>>8)/255.0)
#define BLUE_FLOAT(c)         (((BLUE(c)*(ALPHA(c)+1))>>8)/255.0)
#define ALPHA_FLOAT(c)        (ALPHA(c)/255.0)
#else
#define RED_FLOAT(c)          (RED(c)/255.0)
#define GREEN_FLOAT(c)        (GREEN(c)/255.0)
#define BLUE_FLOAT(c)         (BLUE(c)/255.0)
#define ALPHA_FLOAT(c)        (ALPHA(c)/255.0)
#endif
#endif

#define MAKE_COLOR(r,g,b,a)   (MAKE_RGBA_COLOR(CLAMP01((r))*255,CLAMP01((g))*255,CLAMP01((b))*255,CLAMP01((a))*255))

#define CALC_VIRTUAL_X(pxl,origin,scale,middle,sampl_off)	(((float)((pxl)+(origin)) + (sampl_off)) * (scale) - (middle))
#define CALC_VIRTUAL_Y(pxl,origin,scale,middle,sampl_off)	((-(float)((pxl)+(origin)) - (sampl_off)) * (scale) + (middle))

#define POOLS_ALLOC(s)			(pools_alloc(pools, (s)))
#define ALLOC_CLOSURE_IMAGE(n)		((image_t*)(POOLS_ALLOC(sizeof(image_t) + (n) * sizeof(userval_t))))
#define CLOSURE_IMAGE_ARGS(i)		((userval_t*)(i)->v.closure.args)

#define ALLOC_TUPLE(n)			(POOLS_ALLOC(sizeof(float) * (n)))
#define TUPLE_SET(t,n,x)		((t)[(n)] = (x))
#define TUPLE_NTH(t,n)			((t)[(n)])
#define OUTPUT_TUPLE(t)			((return_tuple = (t)), 0)

#define TUPLE_FROM_COLOR(c)	({ float *tuple = ALLOC_TUPLE(4); \
	    			   TUPLE_SET(tuple, 0, RED_FLOAT((c))); \
	    			   TUPLE_SET(tuple, 1, GREEN_FLOAT((c))); \
	    			   TUPLE_SET(tuple, 2, BLUE_FLOAT((c))); \
	    			   TUPLE_SET(tuple, 3, ALPHA_FLOAT((c))); \
				   tuple; })

#define TUPLE_RED(t)		CLAMP01(TUPLE_NTH((t),0))
#define TUPLE_GREEN(t)		CLAMP01(TUPLE_NTH((t),1))
#define TUPLE_BLUE(t)		CLAMP01(TUPLE_NTH((t),2))
#define TUPLE_ALPHA(t)		CLAMP01(TUPLE_NTH((t),3))

#define APPLY_CURVE(c,p)	((c)->values[(int)(CLAMP01((p)) * (USER_CURVE_POINTS - 1))])
#define APPLY_GRADIENT(g,p)	({ color_t color = (g)->values[(int)(CLAMP01((p)) * (USER_CURVE_POINTS - 1))]; \
	    			   TUPLE_FROM_COLOR(color); })

#define ORIG_VAL(x,y,i,f)     ({ float *result; \
	    			 if ((i)->type == IMAGE_CLOSURE) \
				     result = (i)->v.closure.func(invocation, CLOSURE_IMAGE_ARGS(i), (x), (y), (f), pools); \
				 else { \
				     color_t color = get_orig_val_pixel_func(invocation, (x), (y), (i), (f)); \
				     result = TUPLE_FROM_COLOR(color); \
				 } \
				 result; })

#endif
