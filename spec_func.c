/*
 * spec_func.c
 *
 * MathMap
 *
 * Copyright (C) 2000 Hans Lundmark
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

/* Special functions.
 * At present, only the gamma function for complex arguments.
 * No guarantees about precision!
 */

#include <math.h>
#include <gsl/gsl_complex.h>
#include <gsl/gsl_complex_math.h>

#include "spec_func.h"

#define MAKE_REAL(r)       gsl_complex_rect((r), 0)
#define ADD(a,b)           gsl_complex_add((a),(b))
#define SUB(a,b)           gsl_complex_sub((a),(b))
#define MUL(a,b)           gsl_complex_mul((a),(b))
#define DIV(a,b)           gsl_complex_div((a),(b))

/* Y. L. Luke, 'The Special Functions and Their Approximation', vol II, p 304 */
gsl_complex
cgamma (gsl_complex z)
{
    static double coeff[7] = {41.624436916439068, -51.224241022374774, 11.338755813488977, -0.747732687772388,
			      0.008782877493061, -1.899030264e-6, 1.946335e-9};
    gsl_complex s,H,w;
    int n;

    if(GSL_REAL(z) < 0.0)
	return DIV(cgamma(ADD(z, MAKE_REAL(1.0))), z);
    else
    {
	w = SUB(z, MAKE_REAL(1.0));
	s = MAKE_REAL(coeff[0]);
	H = MAKE_REAL(1.0);
	for(n=1; n<7; n++)
	{
	    H = MUL(H, DIV(SUB(ADD(w, MAKE_REAL(1.0)), MAKE_REAL(n)),
			   ADD(w, MAKE_REAL(n))));
	    s = ADD(s, MUL(MAKE_REAL(coeff[n]), H));
	}
	return MUL(MUL(MUL(MAKE_REAL(2.506628274631),
			   gsl_complex_exp(SUB(MAKE_REAL(-5.5), w))),
		       gsl_complex_pow(ADD(w, MAKE_REAL(5.5)), ADD(w, MAKE_REAL(0.5)))),
		   s);
    }
}
