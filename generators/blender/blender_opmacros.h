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

/* debug tuples are not supported in blender */
#define START_DEBUG_TUPLE(n)       0
#define SET_DEBUG_TUPLE_DATA(i,v)  0

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

#define USERVAL_INT_ACCESS(x)        (cast->userval_ ## x)
#define USERVAL_FLOAT_ACCESS(x)      (cast->userval_ ## x)
#define USERVAL_BOOL_ACCESS(x)       (cast->userval_ ## x)
/* no curves, colors and gradients in blender */
#define USERVAL_CURVE_ACCESS(x,p)    0.0
#define USERVAL_COLOR_ACCESS(x)      0.0
#define USERVAL_GRADIENT_ACCESS(x,p) 0.0

#if NUM_INPUT_DRAWABLES == 1
#define DRAWABLE_SELECT(d)    (ibuf0)
#elif NUM_INPUT_DRAWABLES == 2
#define DRAWABLE_SELECT(d)    ((d) == DRAWABLE0 ? ibuf0 : ibuf1)
#elif NUM_INPUT_DRAWABLES == 3
#define DRAWABLE_SELECT(d)    ((d) == DRAWABLE0 ? ibuf0 : (d) == DRAWABLE1 ? ibuf1 : ibuf2)
#else
#error unsupported number of input drawables
#endif
#define ORIG_VAL(x,y,d,f)     get_orig_val_pixel((x)+X, Y-(y), DRAWABLE_SELECT((d)), (f))

#define RED_FLOAT(c)          (RED(c)/255.0)
#define GREEN_FLOAT(c)        (GREEN(c)/255.0)
#define BLUE_FLOAT(c)         (BLUE(c)/255.0)
#define ALPHA_FLOAT(c)        (ALPHA(c)/255.0)

#define MAKE_COLOR(r,g,b,a)   (MAKE_RGBA_COLOR(CLAMP01((r))*255,CLAMP01((g))*255,CLAMP01((b))*255,CLAMP01((a))*255))
#define OUTPUT_COLOR(c)       ({ p[0] = RED((c)); p[1] = GREEN((c)); p[2] = BLUE((c)); p[3] = ALPHA((c)); 0; })
