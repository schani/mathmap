/*
 * noise.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2000 Mark Probst
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

#include <math.h>
#include <stdlib.h>
#include <assert.h>

#include "noise.h"

#define G_SIZE         256

#define frandom()      (rand() / (float)RAND_MAX)
#define sfrandom()     (frandom() * 2.0 - 1.0)

#define fold(i,j,k)    (p[(p[(p[(i) % G_SIZE] + (j)) % G_SIZE] + (k)) % G_SIZE])
#define drop(t)        (1.0 - 3.0 * (t) * (t) + 2.0 * fabs((t) * (t) * (t)))

float g[G_SIZE][3];
int p[G_SIZE];

static float
quick_wavelet (int i, int j, int k, float u, float v, float w, float sigma)
{
    int f = fold(i, j, k);

    return sigma * (g[f][0] * u + g[f][1] * v + g[f][2] * w);
}

float
noise (float x, float y, float z)
{
    int i, j, k;
    float u, u1, v, v1, w, w1;
    float dropu, dropu1, dropv, dropv1, dropw, dropw1;

    if (x < 0)
	x = x + G_SIZE * ceil(-x / G_SIZE);
    if (y < 0)
	y = y + G_SIZE * ceil(-y / G_SIZE);
    if (z < 0)
	z = z + G_SIZE * ceil(-z / G_SIZE);

    i = (int)floor(x);
    j = (int)floor(y);
    k = (int)floor(z);

    u = x - i; u1 = x - (i + 1);
    v = y - j; v1 = y - (j + 1);
    w = z - k; w1 = z - (k + 1);

    dropu = drop(u); dropu1 = drop(u1);
    dropv = drop(v); dropv1 = drop(v1);
    dropw = drop(w); dropw1 = drop(w1);

    return quick_wavelet(i, j, k, u, v, w, dropu * dropv * dropw)
	+ quick_wavelet(i + 1, j, k, u1, v, w, dropu1 * dropv * dropw)
	+ quick_wavelet(i, j + 1, k, u, v1, w, dropu * dropv1 * dropw)
	+ quick_wavelet(i, j, k + 1, u, v, w1, dropu * dropv * dropw1)
	+ quick_wavelet(i + 1, j + 1, k, u1, v1, w, dropu1 * dropv1 * dropw)
	+ quick_wavelet(i + 1, j, k + 1, u1, v, w1, dropu1 * dropv * dropw1)
	+ quick_wavelet(i, j + 1, k + 1, u, v1, w1, dropu * dropv1 * dropw1)
	+ quick_wavelet(i + 1, j + 1, k + 1, u1, v1, w1, dropu1 * dropv1 * dropw1);
}

void
init_noise (void)
{
    int i;

    for (i = 0; i < G_SIZE; ++i)
    {
	float x, y, z;
	float sql, l;

	do
	{
	    x = sfrandom();
	    y = sfrandom();
	    z = sfrandom();
	    sql = x * x + y * y + z * z;
	} while (sql > 1.0);

	l = sqrt(sql);
	g[i][0] = x / l;
	g[i][1] = y / l;
	g[i][2] = z / l;
    }

    for (i = 0; i < G_SIZE; ++i)
	p[i] = i;
    for (i = 0; i < G_SIZE; ++i)
    {
	int j = random() % G_SIZE;
	int t;

	t = p[i];
	p[i] = p[j];
	p[j] = t;
    }
}
