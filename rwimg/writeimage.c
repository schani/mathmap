/* -*- c -*- */

/*
 * writeimage.c
 *
 * rwimg
 *
 * Copyright (C) 2000-2006 Mark Probst
 * Copyright (C) 2006 Xavier Martin
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
#include <string.h>
#include <strings.h>

#ifdef RWIMG_PNG
#include "rwpng.h"
#endif
#ifdef RWIMG_JPEG
#include "rwjpeg.h"
#endif

#include "writeimage.h"

static int
discover_format (const char *filename)
{
    static struct { const char *suffix; int format; } formats[] =
							  {
#ifdef RWIMG_PNG
							      { ".png", IMAGE_FORMAT_PNG },
#endif
#ifdef RWIMG_JPEG
							      { ".jpg", IMAGE_FORMAT_JPEG },
							      { ".jpeg", IMAGE_FORMAT_JPEG },
#endif
							      { 0, 0 }
							  };

    int filename_len = strlen(filename);
    int i;

    for (i = 0; formats[i].suffix != 0; ++i)
    {
	int suffix_len = strlen(formats[i].suffix);

	if (filename_len >= suffix_len
	    && strcasecmp(formats[i].suffix, filename + filename_len - suffix_len) == 0)
	    return formats[i].format;
    }

    /* no format found - using default (PNG if possible, otherwise JPEG) */
#if defined(RWIMG_PNG)
    return IMAGE_FORMAT_PNG;
#elif defined(RWIMG_JPEG)
    return IMAGE_FORMAT_JPEG;
#else
    return 0;			/* error: no such format available */
#endif
}

image_writer_t*
open_image_writing (const char *filename, int width, int height, int pixel_stride, int row_stride, int format)
{
    image_writer_t *writer;
    void *data = 0;
    image_write_func_t write_func = 0;
    image_writer_free_func_t free_func = 0;

    if (format == IMAGE_FORMAT_AUTO)
    {
	format = discover_format(filename);
	if (format == 0)
	    return 0;
    }

    if (0)
	assert(0);
#ifdef RWIMG_PNG
    else if (format == IMAGE_FORMAT_PNG)
    {
	data = open_png_file_writing(filename, width, height, pixel_stride, row_stride);
	write_func = png_write_lines;
	free_func = png_free_writer_data;
    }
#endif
#ifdef RWIMG_JPEG
    else if(format == IMAGE_FORMAT_JPEG)
    {
	data = open_jpeg_file_writing(filename, width, height);
	write_func = jpeg_write_lines;
	free_func = jpeg_free_writer_data;
    }
#endif
    else
	assert(0);

    if (data == 0)
	return 0;

    writer = (image_writer_t*)malloc(sizeof(image_writer_t));
    writer->width = width;
    writer->height = height;
    writer->num_lines_written = 0;
    writer->data = data;
    writer->write_func = write_func;
    writer->free_func = free_func;

    return writer;
}

void
write_lines (image_writer_t *writer, unsigned char *lines, int num_lines)
{
    assert(writer->num_lines_written + num_lines <= writer->height);

    writer->write_func(writer->data, lines, num_lines);
}

void
free_image_writer (image_writer_t *writer)
{
    writer->free_func(writer->data);
    free(writer);
}

void
write_image (const char *filename, int width, int height, unsigned char *lines,
	     int pixel_stride, int row_stride, int format)
{
    image_writer_t *writer = open_image_writing(filename, width, height, pixel_stride, row_stride, format);

    assert(writer != 0);

    write_lines(writer, lines, height);
    free_image_writer(writer);
}
