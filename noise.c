#include <math.h>
#include <stdlib.h>

#include "noise.h"

typedef struct {
        double x;
        double y;
        double z;
        } Vector;

	/* delta for derivative determination, and its inverse */
#define DELTA		0.001
#define DELTA_INV	1000.

#define B	0x100
#define BM	0xff

#define N	0x100000
#define NP	12   /* 2^N */
#define NM	0xfff

static int p[B + B + 2];
static double g[B + B + 2][3];

	/* cubic spline interpolation */
#define s_curve(t)	( t * t * (3. - 2. * t) )
	/* linear interpolation */
#define lerp(t, a, b)	( a + t * (b - a) )
#define at3(rx,ry,rz)	( rx * q[0] + ry * q[1] + rz * q[2] )
#define at2(rx,ry)	( rx * q[0] + ry * q[1] )
#define setup(u,b0,b1,r0,r1)\
	t = u + N;\
	b0 = ((int)t) & BM;\
	b1 = (b0+1) & BM;\
	r0 = t - (int)t;\
	r1 = r0 - 1.;

void Init_Noise();


float
noise3(float x, float y, float z)
{
	int bx0, bx1, by0, by1, bz0, bz1, b00, b10, b01, b11;
	double rx0, rx1, ry0, ry1, rz0, rz1, *q, sx, sy, sz, t;
	double a, b, c, d, u, v;
	int i, j;

	Vector vec = { x, y, z };

#ifdef UNLIMITED_NOISE_DOMAIN
	filter_args(&vec.x, &vec.y, &vec.z);
#endif

	setup(vec.x, bx0,bx1, rx0,rx1);
	setup(vec.y, by0,by1, ry0,ry1);
	setup(vec.z, bz0,bz1, rz0,rz1);

	i = p[ bx0 ];
	j = p[ bx1 ];

	b00 = p[ i + by0 ];
	b10 = p[ j + by0 ];
	b01 = p[ i + by1 ];
	b11 = p[ j + by1 ];

	sx = s_curve(rx0);
	sy = s_curve(ry0);
	sz = s_curve(rz0);

	q = g[ b00 + bz0 ];	u = at3(rx0,ry0,rz0);
	q = g[ b10 + bz0 ];	v = at3(rx1,ry0,rz0);
	a = lerp(sx, u, v);

	q = g[ b01 + bz0 ];	u = at3(rx0,ry1,rz0);
	q = g[ b11 + bz0 ];	v = at3(rx1,ry1,rz0);
	b = lerp(sx, u, v);

	c = lerp(sy, a, b);		/* interpolate in y at lo z */

	q = g[ b00 + bz1 ];	u = at3(rx0,ry0,rz1);
	q = g[ b10 + bz1 ];	v = at3(rx1,ry0,rz1);
	a = lerp(sx, u, v);

	q = g[ b01 + bz1 ];	u = at3(rx0,ry1,rz1);
	q = g[ b11 + bz1 ];	v = at3(rx1,ry1,rz1);
	b = lerp(sx, u, v);

	d = lerp(sy, a, b);		/* interpolate in y at hi z */

	return 1.5 * lerp(sz, c, d);	/* interpolate in z */
} /* Noise3() */


double
Noise2(vec)
Vector	vec;
{
	int	bx0, bx1, by0, by1, b00, b10, b01, b11;
	double	rx0, rx1, ry0, ry1, *q, sx, sy, a, b, t, u, v, result;
	int i, j;

#ifdef UNLIMITED_NOISE_DOMAIN
	filter_args(&vec.x, &vec.y, &vec.z);
#endif

	setup(vec.x, bx0,bx1, rx0,rx1);
	setup(vec.y, by0,by1, ry0,ry1);

	i = p[ bx0 ];
	j = p[ bx1 ];

	b00 = p[ i + by0 ];
	b10 = p[ j + by0 ];
	b01 = p[ i + by1 ];
	b11 = p[ j + by1 ];

	sx = s_curve(rx0);
	sy = s_curve(ry0);

	q = g[ b00 ];		/* get random gradient */
	u = at2(rx0,ry0);	/* get weight on lo x side (lo y) */
	q = g[ b10 ];
	v = at2(rx1,ry0);	/* get weight on hi x side (lo y) */
	a = lerp(sx, u, v);	/* get value at distance sx between u & v */

				/* similarly at hi y... */
	q = g[ b01 ];	u = at2(rx0,ry1);
	q = g[ b11 ];	v = at2(rx1,ry1);
	b = lerp(sx, u, v);

	result = 1.5 * lerp(sy, a, b);	/* interpolate in y */

	return (result);
}  /* Noise2() */

float
noise1 (float arg)
{
	int bx0, bx1;
	float rx0, rx1, sx, t, u, v;

	setup(arg, bx0,bx1, rx0,rx1);

	sx = s_curve(rx0);

	u = rx0 * g[ p[ bx0 ] ][0];
	v = rx1 * g[ p[ bx1 ] ][0];

	return lerp(sx, u, v);
}


#include "noise_table.c"

void
Init_Noise()
{
	int i;

        for (i= 0; i < B+B+2; ++i)
          {
              p[i]= p_precomputed[i];
              g[i][0]= g_precomputed[i][0];
              g[i][1]= g_precomputed[i][1];
              g[i][2]= g_precomputed[i][2];
          }
}

Vector
vecnoise3 (Vector vec)
{
    Vector result;

    result.x = noise1(vec.x);
    result.y = noise1(vec.y);
    result.z = noise1(vec.z);

    return result;
}

Vector
add_vectors (Vector vec1, Vector vec2)
{
    Vector result;

    result.x = vec1.x + vec2.x;
    result.y = vec1.y + vec2.y;
    result.z = vec1.z + vec2.z;

    return result;
}

/* "Variable Lacunarity Noise"  -or- VLNoise3()
 * A distorted variety of Perlin noise.
 *
 * Copyright 1994 F. Kenton Musgrave 
 */
float
vlnoise3 ( float x, float y, float z, float distortion )
{
        Vector offset;
	Vector point = { x, y, z };

        offset.x = point.x +0.5;        /* misregister domain */
        offset.y = point.y +0.5;
        offset.z = point.z +0.5;

        offset = vecnoise3( offset );   /* get a random vector */

        offset.x *= distortion;         /* scale the randomization */
        offset.y *= distortion;
        offset.z *= distortion;

        /* ``point'' is the domain; distort domain by adding ``offset'' */
        point = add_vectors( point, offset );

        return noise3( point.x, point.y, point.z );         /* distorted-domain noise */

} /* VLNoise3() */

#define FLOOR(x) ((int)(x) - ((x) < 0 && (x) != (int)(x)))
#define CEIL(x) ((int)(x) + ((x) > 0 && (x) != (int)(x)))
#define CLAMP(x,a,b) ((x) =< (a) ? (a) : ((x) >= (b) ? (b) : (x)))
#define LERP(t,x0,x1)  ((x0) + (t)*((x1)-(x0)))

#define PULSE(a,b,x) (step((a),(x)) - step((b),(x)))
#define boxstep(a,b,x) clamp(((x)-(a))/((b)-(a)),0,1)

extern float Abs(float x);	/* the name "abs" is already in use */
extern float bias(float b, float x);
extern float clamp(float x, float a, float b);
extern float gain(float g, float x);
extern float gammacorrect(float gamma, float x);
extern float max(float a, float b);
extern float min(float a, float b);
extern float mod(float a, float b);
extern float smoothstep(float a, float b, float x);
extern float spline(float x, int nknots, float *knot);
extern float step(float a, float x);

extern float gnoise(float x, float y, float z);
extern float gvnoise(float x, float y, float z);
extern float scnoise(float x, float y, float z);
extern float vcnoise(float x, float y, float z);
extern float vnoise(float x, float y, float z);

#define TABSIZE          256
#define TABMASK          (TABSIZE-1)
#define PERM(x)          perm[(x)&TABMASK]
#define INDEX(ix,iy,iz)  PERM((ix)+PERM((iy)+PERM(iz)))

#define RANDMASK  0x7fffffff
#define RANDNBR   ((random() & RANDMASK)/(double) RANDMASK)

extern unsigned char perm[TABSIZE];	/* see perm.c */

extern float catrom2(float d);		/* see catrom2.c */

#define NEXT(h)     (((h)+1) & TABMASK)
#define NIMPULSES   3

static float impulseTab[TABSIZE*4];

static void impulseTabInit(int seed);

unsigned char perm[TABSIZE] = {
        225,155,210,108,175,199,221,144,203,116, 70,213, 69,158, 33,252,
          5, 82,173,133,222,139,174, 27,  9, 71, 90,246, 75,130, 91,191,
        169,138,  2,151,194,235, 81,  7, 25,113,228,159,205,253,134,142,
        248, 65,224,217, 22,121,229, 63, 89,103, 96,104,156, 17,201,129,
         36,  8,165,110,237,117,231, 56,132,211,152, 20,181,111,239,218,
        170,163, 51,172,157, 47, 80,212,176,250, 87, 49, 99,242,136,189,
        162,115, 44, 43,124, 94,150, 16,141,247, 32, 10,198,223,255, 72,
         53,131, 84, 57,220,197, 58, 50,208, 11,241, 28,  3,192, 62,202,
         18,215,153, 24, 76, 41, 15,179, 39, 46, 55,  6,128,167, 23,188,
        106, 34,187,140,164, 73,112,182,244,195,227, 13, 35, 77,196,185,
         26,200,226,119, 31,123,168,125,249, 68,183,230,177,135,160,180,
         12,  1,243,148,102,166, 38,238,251, 37,240,126, 64, 74,161, 40,
        184,149,171,178,101, 66, 29, 59,146, 61,254,107, 42, 86,154,  4,
        236,232,120, 21,233,209, 45, 98,193,114, 78, 19,206, 14,118,127,
         48, 79,147, 85, 30,207,219, 54, 88,234,190,122, 95, 67,143,109,
        137,214,145, 93, 92,100,245,  0,216,186, 60, 83,105, 97,204, 52
};

float
scnoise(float x, float y, float z)
{
    float *fp;
    int i, j, k, h, n;
    int ix, iy, iz;
    float sum = 0;
    float fx, fy, fz, dx, dy, dz, distsq;

    ix = FLOOR(x); fx = x - ix;
    iy = FLOOR(y); fy = y - iy;
    iz = FLOOR(z); fz = z - iz;
    
    /* Perform the sparse convolution. */
    for (i = -2; i <= 2; i++) {
      for (j = -2; j <= 2; j++) {
        for (k = -2; k <= 2; k++) {
            /* Compute voxel hash code. */
            h = INDEX(ix+i,iy+j,iz+k);
            
            for (n = NIMPULSES; n > 0; n--, h = NEXT(h)) {
                /* Convolve filter and impulse. */
                fp = &impulseTab[h*4];
                dx = fx - (i + *fp++);
                dy = fy - (j + *fp++);
                dz = fz - (k + *fp++);
                distsq = dx*dx + dy*dy + dz*dz;
                sum += catrom2(distsq) * *fp;
            }
        }
      }
    }

    return sum / NIMPULSES;
}

static void
impulseTabInit(int seed)
{
    int i;
    float *f = impulseTab;

    srandom(seed); /* Set random number generator seed. */
    for (i = 0; i < TABSIZE; i++) {
        *f++ = RANDNBR;
        *f++ = RANDNBR;
        *f++ = RANDNBR;
        *f++ = 1. - 2.*RANDNBR;
    }
}

float
catrom2(float d)
{
#define SAMPRATE 100  /* table entries per unit distance */
#define NENTRIES (4*SAMPRATE+1)
    float x;
    int i;
    static float table[NENTRIES];
    static int initialized = 0;

    if (d >= 4)
        return 0;

    if (!initialized) {
        for (i = 0; i < NENTRIES; i++) {
            x = i/(float) SAMPRATE;
            x = sqrtf(x);
            if (x < 1)
                table[i] = 0.5 * (2+x*x*(-5+x*3));
            else
                table[i] = 0.5 * (4+x*(-8+x*(5-x)));
        }
        initialized = 1;
    }

    d = d*SAMPRATE + 0.5;
    i = FLOOR(d);
    if (i >= NENTRIES)
        return 0;
    return table[i];
}

void
init_noise (void)
{
    Init_Noise();
    impulseTabInit(665);
}
