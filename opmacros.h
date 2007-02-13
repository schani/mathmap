#define NOP()                 (0.0)
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
                                          invocation->debug_tuples[invocation->num_debug_tuples].length = 0; \
                                          invocation->debug_tuples[invocation->num_debug_tuples].number = (n); \
                                          ++invocation->num_debug_tuples; } \
                                      0; })
/* this assumes that operator calls are not reordered */
#define SET_DEBUG_TUPLE_DATA(i,v)  ({ if (invocation->do_debug && invocation->num_debug_tuples < MAX_DEBUG_TUPLES) { \
                                          invocation->debug_tuples[invocation->num_debug_tuples - 1].data[(int)(i)] = (v); \
                                          invocation->debug_tuples[invocation->num_debug_tuples - 1].length = (i) + 1; } \
                                      0; })

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
#define SOLVE_POLY_2(a,b,c)   0
#define SOLVE_POLY_3(a,b,c,d) 0
#define RAND(a,b)             ((rand() / (float)RAND_MAX) * ((b) - (a)) + (a))
#define CLAMP01(x)            (MAX(0,MIN(1,(x))))
#define USERVAL_INT_ACCESS(x)        (invocation->uservals[(x)].v.int_const)
#define USERVAL_FLOAT_ACCESS(x)      (invocation->uservals[(x)].v.float_const)
#define USERVAL_BOOL_ACCESS(x)       (invocation->uservals[(x)].v.bool_const)
#define USERVAL_CURVE_ACCESS(x,p)    (invocation->uservals[(x)].v.curve.values[(int)(CLAMP01((p)) * (USER_CURVE_POINTS - 1))])
#define USERVAL_COLOR_ACCESS(x)      (invocation->uservals[(x)].v.color.value)
#define USERVAL_GRADIENT_ACCESS(x,p) (invocation->uservals[(x)].v.gradient.values[(int)(CLAMP01((p)) * (USER_GRADIENT_POINTS - 1))])
#ifdef INTERPRETER
#define ORIG_VAL(x,y,d,f)     ({ color_t c; \
                                 if (invocation->antialiasing) \
                                     c = get_orig_val_intersample_pixel(invocation, (x), (y), (d), (f)); \
                                 else \
                                     c = get_orig_val_pixel(invocation, (x), (y), (d), (f)); \
                                 c; })
#else
#define ORIG_VAL(x,y,d,f)     get_orig_val_pixel_func(invocation, (x), (y), (d), (f))
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

#ifdef OPENSTEP
#define MAKE_COLOR(r,g,b,a)   ({ float _a = CLAMP01((a)); MAKE_RGBA_COLOR(CLAMP01((r))*_a*255,CLAMP01((g))*_a*255,CLAMP01((b))*_a*255,_a*255); })
#define OUTPUT_COLOR(c)       ({ (*(color_t*)p = (c)); 0; })
#else
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
