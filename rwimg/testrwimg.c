/*
 * testrwimg.c
 *
 * rwimg
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

#include <stdio.h>
#include <assert.h>

#include "readimage.h"
#include "writeimage.h"

int
main (int argc, char *argv[])
{
    unsigned char *data;
    int width, height;

    if (argc != 3) {
	fprintf(stderr, "Usage: testrwimg <in> <out>\n");
	return 1;
    }

    data = read_image(argv[1], &width, &height);
    assert(data != NULL);

    write_image(argv[2], width, height, data, 3, width * 3, IMAGE_FORMAT_AUTO);

    return 0;
}
