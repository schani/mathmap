/*
 * builtins/libnoise.h
 *
 * MathMap
 *
 * Copyright (C) 2009 Mark Probst
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

#ifndef __LIBNOISE_H__
#define __LIBNOISE_H__

#ifdef __cplusplus
extern "C" {
#endif

/* TEMPLATE libnoise */
extern float libnoise_perlin (int num_octaves, float persistence, float lacunarity,
			      float x, float y, float z);
extern float libnoise_billow (int num_octaves, float persistence, float lacunarity,
			      float x, float y, float z);
extern float libnoise_ridged_multi (int num_octaves, float lacunarity,
				    float x, float y, float z);
extern float libnoise_voronoi (float displacement, float x, float y, float z);
/* END */

#ifdef __cplusplus
}
#endif

#endif
