/* -*- c -*- */

/*
 * rwpng.c
 *
 * rwimg
 *
 * Copyright (C) 1997-2004 Mark Probst
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

#include <assert.h>
#include <stdlib.h>

#include <png.h>

#include "rwpng.h"

typedef struct
{
    FILE *file;
    png_structp png_ptr;
    png_infop info_ptr, end_info;
    int row_stride;
    int pixel_stride;
    int have_read;
} png_data_t;

void*
open_png_file_reading (const char *filename, int *width, int *height)
{
    png_data_t *data = (png_data_t*)malloc(sizeof(png_data_t));
    int bit_depth, color_type;

    assert(data != 0);

    data->file = fopen(filename, "rb");
    assert(data->file != 0);

    data->png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
    assert(data->png_ptr != 0);

    data->info_ptr = png_create_info_struct(data->png_ptr);
    assert(data->info_ptr != 0);

    data->end_info = png_create_info_struct(data->png_ptr);
    assert(data->end_info != 0);

    if (setjmp (png_jmpbuf (data->png_ptr)))
	assert(0);

    png_init_io(data->png_ptr, data->file);

    png_read_info(data->png_ptr, data->info_ptr);

    *width = png_get_image_width (data->png_ptr, data->info_ptr);
    *height = png_get_image_height (data->png_ptr, data->info_ptr);

    bit_depth = png_get_bit_depth (data->png_ptr, data->info_ptr);
    if (bit_depth != 8 && bit_depth != 16)
    {
	fprintf(stderr, "PNG files are only supported with bit depths 8 and 16.\n");
	/* FIXME: free stuff */
	return 0;
    }

    color_type = png_get_color_type (data->png_ptr, data->info_ptr);
    if (color_type != PNG_COLOR_TYPE_RGB
	&& color_type != PNG_COLOR_TYPE_RGB_ALPHA
	&& color_type != PNG_COLOR_TYPE_GRAY
	&& color_type != PNG_COLOR_TYPE_GRAY_ALPHA)
    {
	fprintf(stderr, "PNG files are only supported in RGB and Gray, with or without alpha.\n");
	/* FIXME: free stuff */
	return 0;
    }

    if (png_get_interlace_type (data->png_ptr, data->info_ptr) != PNG_INTERLACE_NONE)
    {
	fprintf(stderr, "Interlaced PNG files are not supported.\n");
	/* FIXME: free stuff */
	return 0;
    }

    data->have_read = 0;

    return data;
}

void
png_read_lines (void *_data, unsigned char *lines, int num_lines)
{
    png_data_t *data = (png_data_t*)_data;
    int i;
    int bps, spp;
    unsigned char *row;
    int color_type, width;

    if (setjmp (png_jmpbuf (data->png_ptr)))
	assert(0);

    color_type = png_get_color_type (data->png_ptr, data->info_ptr);
    if (color_type == PNG_COLOR_TYPE_GRAY)
	spp = 1;
    else if (color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
	spp = 2;
    else if (color_type == PNG_COLOR_TYPE_RGB)
	spp = 3;
    else
	spp = 4;

    if (png_get_bit_depth (data->png_ptr, data->info_ptr) == 16)
	bps = 2;
    else
	bps = 1;

    width = png_get_image_width (data->png_ptr, data->info_ptr);
    row = (unsigned char*)malloc (width * spp * bps);

    for (i = 0; i < num_lines; ++i)
    {
	int j, channel;

	png_read_row(data->png_ptr, (png_bytep)row, 0);

	if (spp <= 2)
	    for (j = 0; j < width; ++j)
		for (channel = 0; channel < 3; ++channel)
		    lines[i * width * 3 + j * 3 + channel] = row[j * spp * bps];
	else
	    for (j = 0; j < width; ++j)
		for (channel = 0; channel < 3; ++channel)
		    lines[i * width * 3 + j * 3 + channel]
			= row[j * spp * bps + channel * bps];
    }

    free(row);

    data->have_read = 1;
}

void
png_free_reader_data (void *_data)
{
    png_data_t *data = (png_data_t*)_data;

    if (setjmp (png_jmpbuf (data->png_ptr)))
	assert(0);

    if (data->have_read)
	png_read_end(data->png_ptr, data->end_info);
    png_destroy_read_struct(&data->png_ptr, &data->info_ptr, &data->end_info);
    fclose(data->file);

    free(data);
}

void*
open_png_file_writing (const char *filename, int width, int height, int pixel_stride, int row_stride)
{
    png_data_t *data = (png_data_t*)malloc(sizeof(png_data_t));

    assert(data != 0);

    assert(pixel_stride >= 3);

    data->file = fopen(filename, "wb");
    assert(data->file != 0);

    data->png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
    assert(data->png_ptr != 0);

    data->info_ptr = png_create_info_struct(data->png_ptr);
    assert(data->info_ptr != 0);

    if (setjmp (png_jmpbuf (data->png_ptr)))
	assert(0);

    if (pixel_stride == 4)
	png_set_filler(data->png_ptr, 0, PNG_FILLER_AFTER);

    png_init_io(data->png_ptr, data->file);

    png_set_IHDR (data->png_ptr, data->info_ptr,
		  width, height, 8, PNG_COLOR_TYPE_RGB,
		  PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

    png_write_info(data->png_ptr, data->info_ptr);

    data->pixel_stride = pixel_stride;
    data->row_stride = row_stride;

    return data;
}

void
png_write_lines (void *_data, unsigned char *lines, int num_lines)
{
    png_data_t *data = (png_data_t*)_data;
    unsigned char *packed_line;
    int i;
    int width;

    if (setjmp (png_jmpbuf (data->png_ptr)))
	assert(0);

    width = png_get_image_width (data->png_ptr, data->info_ptr);
    if (data->pixel_stride != 3)
    {
	packed_line = (unsigned char*)malloc(width * 3);
	assert(packed_line != 0);
    }
    else
	packed_line = 0;

    for (i = 0; i < num_lines; ++i)
    {
	unsigned char *p = lines + i * data->row_stride;

	if (packed_line != 0)
	{
	    int j;

	    for (j = 0; j < width; ++j)
	    {
		packed_line[j * 3 + 0] = p[j * data->pixel_stride + 0];
		packed_line[j * 3 + 1] = p[j * data->pixel_stride + 1];
		packed_line[j * 3 + 2] = p[j * data->pixel_stride + 2];
	    }

	    p = packed_line;
	}

	png_write_row(data->png_ptr, (png_bytep)p);
    }

    if (packed_line != 0)
	free(packed_line);
}

void
png_free_writer_data (void *_data)
{
    png_data_t *data = (png_data_t*)_data;

    if (setjmp (png_jmpbuf (data->png_ptr)))
	assert(0);

    png_write_end(data->png_ptr, data->info_ptr);
    png_destroy_write_struct(&data->png_ptr, &data->info_ptr);
    fclose(data->file);

    free(data);
}
