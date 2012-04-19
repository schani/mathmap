/*
 * builtins/libnoise.cpp
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

#include <noise.h>

extern "C"
{
#include "../mathmap.h"
}

#include "libnoise.h"

using namespace noise;

extern "C"
CALLBACK_SYMBOL
float
libnoise_perlin (int num_octaves, float persistence, float lacunarity,
		 float x, float y, float z)
{
    module::Perlin p;

    p.SetNoiseQuality (QUALITY_BESTEST);
    p.SetOctaveCount (num_octaves);
    p.SetLacunarity (lacunarity);
    p.SetPersistence (persistence);

    return p.GetValue (x, y, z);
}

extern "C"
CALLBACK_SYMBOL
float
libnoise_billow (int num_octaves, float persistence, float lacunarity,
		 float x, float y, float z)
{
    module::Billow p;

    p.SetNoiseQuality (QUALITY_BESTEST);
    p.SetOctaveCount (num_octaves);
    p.SetLacunarity (lacunarity);
    p.SetPersistence (persistence);

    return p.GetValue (x, y, z);
}

extern "C"
CALLBACK_SYMBOL
float
libnoise_ridged_multi (int num_octaves, float lacunarity,
		       float x, float y, float z)
{
    module::RidgedMulti p;

    p.SetNoiseQuality (QUALITY_BESTEST);
    p.SetOctaveCount (num_octaves);
    p.SetLacunarity (lacunarity);

    return p.GetValue (x, y, z);
}

extern "C"
CALLBACK_SYMBOL
float
libnoise_voronoi (float displacement, float x, float y, float z)
{
    module::Voronoi p;

    p.SetDisplacement (displacement);

    return p.GetValue (x, y, z);
}
