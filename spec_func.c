/*
 * spec_func.c
 *
 * MathMap
 *
 * Copyright (C) 2000 Hans Lundmark
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

/* Special functions.
 * At present, only the gamma function for complex arguments.
 * No guarantees about precision!
 */

#include <math.h>
#include <complex.h>

#include "spec_func.h"

/* Y. L. Luke, 'The Special Functions and Their Approximation', vol II, p 304 */
complex float
cgamma (complex float z)
{
    static double coeff[7] = {41.624436916439068, -51.224241022374774, 11.338755813488977, -0.747732687772388,
			      0.008782877493061, -1.899030264e-6, 1.946335e-9};
    complex double s,H,w;
    int n;

    if(creal(z) < 0.0)
    {
	complex double denom = 1.0;
	int flr = -floor(creal(z));

	for (n = 0; n < flr; ++n)
	    denom = denom * (z + n);

	return cgamma(z + flr) / denom;
    }
    else
    {
	w = z - 1.0;
	s = coeff[0];
	H=1.0;
	for(n=1; n<7; n++) {
	    H *= (w+1-n) / (w+n);
	    s += coeff[n] * H;
	}
	return( 2.506628274631 * cexp(-w-5.5) * cpow(w+5.5,w+0.5) * s );
    }
}

#ifdef TEST_CGAMMA
#include <stdlib.h>
#include <stdio.h>

int
main (int argc, char *argv[])
{
    complex float result;

    if (argc != 3)
    {
	fprintf(stderr, "usage: %s <r> <i>\n", argv[0]);
	return 1;
    }

    result = cgamma(atof(argv[1]) + I * atof(argv[2]));

    printf("%f + %f * I\n", crealf(result), cimag(result));

    return 0;
}

#endif
