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

#define frandom()      (random() / (float)0x7fffffff)
#define sfrandom()     (frandom() * 2.0 - 1.0)

#define fold(i,j,k)    (p[(p[(p[(i) % G_SIZE] + (j)) % G_SIZE] + (k)) % G_SIZE])
#define drop(t)        (1.0 - 3.0 * (t) * (t) + 2.0 * fabs((t) * (t) * (t)))

float g[G_SIZE][3];
int p[G_SIZE];

static float
wavelet (int i, int j, int k, float x, float y, float z)
{
    float u, v, w;
    float sigma;
    int f;

    u = x - i;
    v = y - j;
    w = z - k;

    sigma = drop(u) * drop(v) * drop(w);

    f = fold(i, j, k);

    return sigma * (g[f][0] * u + g[f][1] * v + g[f][2] * w);
}

float
noise (float x, float y, float z)
{
    int i, j, k;

    if (x < 0)
	x = x + G_SIZE * ceil(-x / G_SIZE);
    if (y < 0)
	y = y + G_SIZE * ceil(-y / G_SIZE);
    if (z < 0)
	z = z + G_SIZE * ceil(-z / G_SIZE);

    i = (int)floor(x);
    j = (int)floor(y);
    k = (int)floor(z);

    return wavelet(i, j, k, x, y, z)
	+ wavelet(i + 1, j, k, x, y, z)
	+ wavelet(i, j + 1, k, x, y, z)
	+ wavelet(i, j, k + 1, x, y, z)
	+ wavelet(i + 1, j + 1, k, x, y, z)
	+ wavelet(i + 1, j, k + 1, x, y, z)
	+ wavelet(i, j + 1, k + 1, x, y, z)
	+ wavelet(i + 1, j + 1, k + 1, x, y, z);
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
