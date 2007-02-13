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

/* debug tuples are not supported in pixeltree */
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

#define MAKE_COLOR(r,g,b,a)	        ((pt_pixel_t){ .v = { (r), (g), (b), (a) } })
#define RED(c)				((c).v[0])
#define GREEN(c)			((c).v[1])
#define BLUE(c)				((c).v[2])
#define ALPHA(c)			((c).v[3])

#define USERVAL_INT_ACCESS(x)        (node->input_values[x].integer)
#define USERVAL_FLOAT_ACCESS(x)      (node->input_values[x].real)
#define USERVAL_BOOL_ACCESS(x)       (node->input_values[x].boolean)
#define USERVAL_COLOR_ACCESS(x)      (node->input_values[x].color)
/* FIXME: no curves and gradients yet */
#define USERVAL_CURVE_ACCESS(x,p)    0.0
#define USERVAL_GRADIENT_ACCESS(x,p) 0.0

#define ORIG_VAL(x,y,d,f)	({ pt_coords_t __c = { .v = { (x), (y) } }; \
				   pt_pixel_t __p; \
				   pt_call_node(node->input_values[(d)].node, &__c, &__p); \
				   __p; })
#define OUTPUT_COLOR(c)		((*result = (c)), 0)
