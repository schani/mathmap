/* -*- c -*- */

/*
 * mathmap_common.c
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
#include <assert.h>

#include "internals.h"
#include "tags.h"

#include "mathmap_common.h"

static internal_t *xy_internal = 0, *ra_internal = 0;

double currentX,
    currentY,
    currentR,
    currentA,
    currentT,
    imageR,
    imageX,
    imageY,
    imageW,
    imageH,
    middleX,
    middleY;
int originX,
    originY;

int edge_behaviour_color = 1,
    edge_behaviour_wrap = 2,
    edge_behaviour_reflect = 3;
int edge_behaviour_mode = 1;

int intersamplingEnabled,
    oversamplingEnabled;

int outputBPP;

void
calc_ra (void)
{
    if (ra_internal->is_used)
    {
	double x = currentX,
	    y = currentY;

	currentR = hypot(x, y);
	if (currentR == 0.0)
	    currentA = 0.0;
	else
	    currentA = acos(x / currentR) * 180 / M_PI;

	if (y < 0)
	    currentA = 360 - currentA;
    }
}

void
init_internals (void)
{
    xy_internal = register_internal("xy", xy_tag_number, 2);
    ra_internal = register_internal("ra", ra_tag_number, 2);
    register_internal("t", nil_tag_number, 1);
    register_internal("XY", xy_tag_number, 2);
    register_internal("WH", xy_tag_number, 2);
    register_internal("R", nil_tag_number, 1);
}

void
update_image_internals (void)
{
    internal_t *internal;
    tuple_info_t dummy;

    internal = lookup_internal("t", &dummy);
    internal->value.data[0] = currentT;

    internal = lookup_internal("XY", &dummy);
    internal->value.data[0] = imageX;
    internal->value.data[1] = imageY;
    
    internal = lookup_internal("WH", &dummy);
    internal->value.data[0] = imageW;
    internal->value.data[1] = imageH;
    
    internal = lookup_internal("R", &dummy);
    internal->value.data[0] = imageR;
}

void
update_pixel_internals (void)
{
    xy_internal->value.data[0] = currentX;
    xy_internal->value.data[1] = currentY;

    ra_internal->value.data[0] = currentR;
    ra_internal->value.data[1] = currentA;
}

void
write_tuple_to_pixel (tuple_t *tuple, guchar *dest)
{
    float redf,
	greenf,
	bluef,
	alphaf;

    tuple_to_color(tuple, &redf, &greenf, &bluef, &alphaf);

    if (outputBPP == 1 || outputBPP == 2)
	dest[0] = (0.299 * redf + 0.587 * greenf + 0.114 * bluef) * 255;
    else if (outputBPP == 3 || outputBPP == 4)
    {
	dest[0] = redf * 255;
	dest[1] = greenf * 255;
	dest[2] = bluef * 255;
    }
    else
	assert(0);

    if (outputBPP == 2 || outputBPP == 4)
	dest[outputBPP - 1] = alphaf * 255;
}
