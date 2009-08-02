/* -*- c -*- */

/*
 * floatmap.c
 *
 * MathMap
 *
 * Copyright (C) 2008 Mark Probst
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

#include <string.h>

#include "drawable.h"
#include "rwimg/writeimage.h"

image_t*
floatmap_alloc (int width, int height, mathmap_pools_t *pools)
{
    image_t *img = mathmap_pools_alloc(pools, sizeof(image_t));

    img->type = IMAGE_FLOATMAP;
    img->pixel_width = width;
    img->pixel_height = height;
    img->v.floatmap.ax = img->v.floatmap.bx = (float)(width - 1) / 2.0;
    img->v.floatmap.ay = img->v.floatmap.by = (float)(height - 1) / 2.0;
    img->v.floatmap.ay *= -1.0;

    img->v.floatmap.data = mathmap_pools_alloc(pools, sizeof(float) * width * height * NUM_FLOATMAP_CHANNELS);

    return img;
}

image_t*
floatmap_copy (image_t *floatmap, mathmap_pools_t *pools)
{
    image_t *copy;

    g_assert(floatmap->type == IMAGE_FLOATMAP);

    copy = floatmap_alloc(floatmap->pixel_width, floatmap->pixel_height, pools);

    copy->v.floatmap.ax = floatmap->v.floatmap.ax;
    copy->v.floatmap.bx = floatmap->v.floatmap.bx;
    copy->v.floatmap.ay = floatmap->v.floatmap.ay;
    copy->v.floatmap.by = floatmap->v.floatmap.by;

    memcpy(copy->v.floatmap.data, floatmap->v.floatmap.data,
	   sizeof(float) * floatmap->pixel_width * floatmap->pixel_height * NUM_FLOATMAP_CHANNELS);

    return copy;
}

void
floatmap_get_channel_column (float *dst, image_t *img, int col, int channel)
{
    int i;

    g_assert(img->type == IMAGE_FLOATMAP);
    g_assert(col >= 0 && col < img->pixel_width);
    g_assert(channel >= 0 && channel < NUM_FLOATMAP_CHANNELS);

    for (i = 0; i < img->pixel_height; ++i)
	dst[i] = FLOATMAP_VALUE_XY(img, col, i, channel);
}

void
floatmap_get_channel_row (float *dst, image_t *img, int row, int channel)
{
    int i;

    g_assert(img->type == IMAGE_FLOATMAP);
    g_assert(row >= 0 && row < img->pixel_height);
    g_assert(channel >= 0 && channel < NUM_FLOATMAP_CHANNELS);

    for (i = 0; i < img->pixel_width; ++i)
	dst[i] = FLOATMAP_VALUE_XY(img, i, row, channel);
}

void
floatmap_set_channel_column (image_t *img, int col, int channel, float *src)
{
    int i;

    g_assert(img->type == IMAGE_FLOATMAP);
    g_assert(col >= 0 && col < img->pixel_width);
    g_assert(channel >= 0 && channel < NUM_FLOATMAP_CHANNELS);

    for (i = 0; i < img->pixel_height; ++i)
	FLOATMAP_VALUE_XY(img, col, i, channel) = src[i];
}

void
floatmap_set_channel_row (image_t *img, int row, int channel, float *src)
{
    int i;

    g_assert(img->type == IMAGE_FLOATMAP);
    g_assert(row >= 0 && row < img->pixel_height);
    g_assert(channel >= 0 && channel < NUM_FLOATMAP_CHANNELS);

    for (i = 0; i < img->pixel_width; ++i)
	FLOATMAP_VALUE_XY(img, i, row, channel) = src[i];
}

void
floatmap_get_column (float *dst, image_t *img, int col)
{
    int i;

    g_assert(img->type == IMAGE_FLOATMAP);
    g_assert(col >= 0 && col < img->pixel_width);

    for (i = 0; i < img->pixel_height; ++i)
	memcpy(dst + i * NUM_FLOATMAP_CHANNELS,
	       &FLOATMAP_VALUE_XY(img, col, i, 0),
	       sizeof(float) * NUM_FLOATMAP_CHANNELS);
}

void
floatmap_get_row (float *dst, image_t *img, int row)
{
    g_assert(img->type == IMAGE_FLOATMAP);
    g_assert(row >= 0 && row < img->pixel_height);

    memcpy(dst, &FLOATMAP_VALUE_XY(img, 0, row, 0),
	   sizeof(float) * img->pixel_width * NUM_FLOATMAP_CHANNELS);
}

void
floatmap_set_column (image_t *img, int col, float *src)
{
    int i;

    g_assert(img->type == IMAGE_FLOATMAP);
    g_assert(col >= 0 && col < img->pixel_width);

    for (i = 0; i < img->pixel_height; ++i)
	memcpy(&FLOATMAP_VALUE_XY(img, col, i, 0),
	       src + i * NUM_FLOATMAP_CHANNELS,
	       sizeof(float) * NUM_FLOATMAP_CHANNELS);
}

void
floatmap_set_row (image_t *img, int row, float *src)
{
    g_assert(img->type == IMAGE_FLOATMAP);
    g_assert(row >= 0 && row < img->pixel_height);

    memcpy(&FLOATMAP_VALUE_XY(img, 0, row, 0), src,
	   sizeof(float) * img->pixel_width * NUM_FLOATMAP_CHANNELS);
}

/* This is for debugging purposes only. */
void
floatmap_write (image_t *img, const char *filename)
{
    unsigned char *data;
    int i;

    g_assert(img->type == IMAGE_FLOATMAP);

    data = g_malloc(3 * img->pixel_width * img->pixel_height);
    for (i = 0; i < img->pixel_width * img->pixel_height; ++i)
    {
	data[i * 3 + 0] = FLOATMAP_VALUE_I(img, i, 0) * 255.0;
	data[i * 3 + 1] = FLOATMAP_VALUE_I(img, i, 1) * 255.0;
	data[i * 3 + 2] = FLOATMAP_VALUE_I(img, i, 2) * 255.0;
    }

    write_image(filename, img->pixel_width, img->pixel_height, data, 3, 3 * img->pixel_width, IMAGE_FORMAT_PNG);

    g_free(data);
}
