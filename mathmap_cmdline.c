/* -*- c -*- */

/*
 * mathmap_cmdline.c
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

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef MOVIES
#include <quicktime.h>
#endif

#include "getopt.h"

#include "exprtree.h"
#include "builtins.h"
#include "postfix.h"
#include "tags.h"
#include "scanner.h"
#include "vars.h"
#include "userval.h"
#include "internals.h"
#include "macros.h"
#include "jump.h"
#include "mathmap.h"
#include "noise.h"
#ifdef USE_CGEN
#include "cgen.h"
#endif
#include "mathmap_common.h"
#include "readimage.h"
#include "writeimage.h"

#ifdef USE_CGEN
#define EVAL_EXPR     eval_c_code
#else
#define EVAL_EXPR     eval_postfix
#endif

extern int mmparse (void);

typedef struct
{
    int drawable_index;
    int frame;
    guchar *data;
    int timestamp;
} cache_entry_t;

static int cache_size = 8;
static cache_entry_t *cache = 0;
static int current_time = 0;

#define DRAWABLE_IMAGE     1
#define DRAWABLE_MOVIE     2

typedef struct
{
    int type;
    cache_entry_t **cache_entries;
    int num_frames;
    union
    {
	char *image_filename;
#ifdef MOVIES
	quicktime_t *movie;
#endif
    } v;
} input_drawable_t;

#define MAX_INPUT_DRAWABLES 64

static input_drawable_t input_drawables[MAX_INPUT_DRAWABLES];
static int num_input_drawables = 0;

exprtree *theExprtree = 0;

char error_string[1024];

int img_width, img_height;

guchar edge_color[4] = { 0, 0, 0, 0 };

int num_gradient_samples = 1;
tuple_t gradient_samples[1];

void
mathmap_get_pixel (int drawable_index, int frame, int x, int y, guchar *pixel)
{
    guchar *p;
    int i;
    input_drawable_t *drawable;

    if (drawable_index < 0 || drawable_index >= num_input_drawables
	|| x < 0 || x >= img_width
	|| y < 0 || y >= img_height
	|| frame < 0 || frame >= input_drawables[drawable_index].num_frames)
    {
	for (i = 0; i < 4; ++i)
	    pixel[i] = edge_color[i];
	return;
    }

    drawable = &input_drawables[drawable_index];

    if (drawable->cache_entries[frame] == 0)
    {
	int lru_index = -1;

	for (i = 0; i < cache_size; ++i)
	    if (cache[i].drawable_index < 0)
	    {
		lru_index = i;
		break;
	    }
	    else
		if (lru_index < 0 || cache[i].timestamp < cache[lru_index].timestamp)
		    lru_index = i;

	if (cache[lru_index].drawable_index >= 0)
	    input_drawables[cache[lru_index].drawable_index].cache_entries[cache[lru_index].frame] = 0;

	if (drawable->type == DRAWABLE_IMAGE)
	{
	    int width, height;

	    if (cache[lru_index].data != 0)
		free(cache[lru_index].data);

	    cache[lru_index].data = read_image(drawable->v.image_filename, &width, &height);
	    assert(width == img_width && height == img_height);
	}
#ifdef MOVIES
	else
	{
	    guchar **rows = (guchar**)malloc(sizeof(guchar*) * img_height);

	    if (cache[lru_index].data == 0)
		cache[lru_index].data = (guchar*)malloc(img_width * img_height * 3);

	    for (i = 0; i < img_height; ++i)
		rows[i] = cache[lru_index].data + i * img_width * 3;

	    quicktime_set_video_position(drawable->v.movie, frame, 0);
	    quicktime_decode_video(drawable->v.movie, rows, 0);

	    free(rows);
	}
#endif

	cache[lru_index].drawable_index = drawable_index;
	cache[lru_index].frame = frame;
	cache[lru_index].timestamp = current_time;

	drawable->cache_entries[frame] = &cache[lru_index];
    }
    else
	drawable->cache_entries[frame]->timestamp = current_time;

    p = drawable->cache_entries[frame]->data + 3 * (img_width * y + x);

    for (i = 0; i < 3; ++i)
	pixel[i] = p[i];
    pixel[3] = 255;

    /*
    if (drawable->bpp == 1 || drawable->bpp == 2)
	pixel[0] = pixel[1] = pixel[2] = p[0];
    else if (drawable->bpp == 3 || drawable->bpp == 4)
	for (i = 0; i < 3; ++i)
	    pixel[i] = p[i];
    else
	assert(0);

    if (drawable->bpp == 1 || drawable->bpp == 3)
	pixel[3] = 255;
    else
        pixel[3] = p[drawable->bpp - 1];
    */
}

int
generate_code (char *expr)
{
    static int result;

    result = 1;

    untag_uservals();

    DO_JUMP_CODE {
	clear_all_variables();
	internals_clear_used();
	scanFromString(expr);
	mmparse();
	endScanningFromString();

	assert(theExprtree != 0);

	if (theExprtree->result.number != rgba_tag_number || theExprtree->result.length != 4)
	{
	    sprintf(error_string, "The expression must have the result type rgba:4.");
	    JUMP(1);
	}

#ifdef USE_CGEN
	assert(gen_and_load_c_code(theExprtree));
#else
	make_postfix(theExprtree);
	output_postfix();
#endif
    } WITH_JUMP_HANDLER {
	result = 0;
    } END_JUMP_HANDLER;

    clear_untagged_uservals();
    untag_uservals();

    update_image_internals();

    return result;
}

void
usage (void)
{
    printf("Usage:\n"
	   "  mathmap --version\n"
	   "      print out version number\n"
	   "  mathmap --help\n"
	   "      print this help text\n"
	   "  mathmap [option ...] <expression> <outfile>\n"
	   "      transform one or more inputs with <expression> and write\n"
	   "      the result to <outfile>\n"
	   "Options:\n"
	   "  -I, --image=FILENAME        input image FILENAME\n"
#ifdef MOVIES
	   "  -M, --movie=FILENAME        input movie FILENAME\n"
	   "  -f, --frames=NUM            output movie has NUM frames\n"
#endif
	   "  -i, --intersampling         use intersampling\n"
	   "  -o, --oversampling          use oversampling\n"
	   "  -c, --cache=NUM             cache NUM input images (default %d)\n"
	   "\n"
	   "Report bugs and suggestions to schani@complang.tuwien.ac.at\n",
	   cache_size);
}

int
main (int argc, char *argv[])
{
    guchar *output, *dest;
    int row, col;
    int i;
    int num_frames = 1;
#ifdef MOVIES
    int generate_movie = 0;
    quicktime_t *output_movie;
    guchar **rows;
#endif

    intersamplingEnabled = 0;
    oversamplingEnabled = 0;

    for (;;)
    {
	static struct option long_options[] =
	    {
		{ "version", no_argument, 0, 256 },
		{ "help", no_argument, 0, 257 },
		{ "intersampling", no_argument, 0, 'i' },
		{ "oversampling", no_argument, 0, 'o' },
		{ "cache", required_argument, 0, 'c' },
		{ "image", required_argument, 0, 'I' },
#ifdef MOVIES
		{ "frames", required_argument, 0, 'f' },
		{ "movie", required_argument, 0, 'M' },
#endif
		{ 0, 0, 0, 0 }
	    };

	int option, option_index;

	option = getopt_long(argc, argv, 
#ifdef MOVIES
			     "iof:I:M:c:", 
#else
			     "ioI:c:",
#endif
			     long_options, &option_index);

	if (option == -1)
	    break;

	switch (option)
	{
	    case 256 :
		printf("MathMap " MATHMAP_VERSION "\n"
		       "\n"
		       "Copyright (C) 1997-2001 Mark Probst\n"
		       "\n"
		       "This program is free software; you can redistribute it and/or modify\n"
		       "it under the terms of the GNU General Public License as published by\n"
		       "the Free Software Foundation; either version 2 of the License, or\n"
		       "(at your option) any later version.\n"
		       "\n"
		       "This program is distributed in the hope that it will be useful,\n"
		       "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
		       "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
		       "GNU General Public License for more details.\n"
		       "\n"
		       "You should have received a copy of the GNU General Public License\n"
		       "along with this program; if not, write to the Free Software\n"
		       "Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.\n");
		return 0;

	    case 257 :
		usage();
		return 0;

	    case 'i' :
		intersamplingEnabled = 1;
		break;

	    case 'o' :
		oversamplingEnabled = 1;
		break;

	    case 'c' :
		cache_size = atoi(optarg);
		assert(cache_size > 0);
		break;

	    case 'I' :
		input_drawables[num_input_drawables].type = DRAWABLE_IMAGE;
		input_drawables[num_input_drawables].cache_entries = (cache_entry_t**)malloc(sizeof(cache_entry_t*));
		input_drawables[num_input_drawables].cache_entries[0] = 0;
		input_drawables[num_input_drawables].num_frames = 1;
		input_drawables[num_input_drawables].v.image_filename = optarg;
		++num_input_drawables;
		break;

#ifdef MOVIES
	    case 'f' :
		generate_movie = 1;
		num_frames = atoi(optarg);
		assert(num_frames > 0);
		break;

	    case 'M' :
		input_drawables[num_input_drawables].type = DRAWABLE_MOVIE;
		input_drawables[num_input_drawables].v.movie = quicktime_open(optarg, 1, 0);
		assert(input_drawables[num_input_drawables].v.movie != 0);
		assert(quicktime_video_tracks(input_drawables[num_input_drawables].v.movie));
		assert(quicktime_video_depth(input_drawables[num_input_drawables].v.movie, 0) == 24);
		input_drawables[num_input_drawables].num_frames = quicktime_video_length(input_drawables[num_input_drawables].v.movie, 0);
		input_drawables[num_input_drawables].cache_entries
		    = (cache_entry_t**)malloc(sizeof(cache_entry_t*) * input_drawables[num_input_drawables].num_frames);
		for (i = 0; i < input_drawables[num_input_drawables].num_frames; ++i)
		    input_drawables[num_input_drawables].cache_entries[i] = 0;
		++num_input_drawables;
		break;
#endif
	}
    }

    if (argc - optind != 2)
    {
	usage();
	return 1;
    }

    init_builtins();
    init_tags();
    init_internals();
    init_macros();
    init_noise();

    assert(num_input_drawables > 0);

    cache = (cache_entry_t*)malloc(sizeof(cache_entry_t) * cache_size);
    for (i = 0; i < cache_size; ++i)
    {
	cache[i].drawable_index = -1;
	cache[i].data = 0;
    }

    if (input_drawables[0].type == DRAWABLE_IMAGE)
    {
	cache[0].drawable_index = 0;
	cache[0].frame = 0;
	cache[0].data = read_image(input_drawables[0].v.image_filename, &img_width, &img_height);
	assert(cache[0].data != 0);
	cache[0].timestamp = current_time;
	input_drawables[0].cache_entries[0] = &cache[0];
    }
#ifdef MOVIES
    else
    {
	img_width = quicktime_video_width(input_drawables[0].v.movie, 0);
	img_height = quicktime_video_height(input_drawables[0].v.movie, 0);
    }
#endif

#ifdef MOVIES
    for (i = 1; i < num_input_drawables; ++i)
	if (input_drawables[i].type == DRAWABLE_MOVIE)
	{
	    assert(quicktime_video_width(input_drawables[i].v.movie, 0) == img_width);
	    assert(quicktime_video_height(input_drawables[i].v.movie, 0) == img_height);
	}
#endif

    outputBPP = 3;
    originX = originY = 0;

    imageW = img_width;
    imageH = img_height;

    middleX = img_width / 2.0;
    middleY = img_height / 2.0;

    if (middleX > img_width - middleX)
	imageX = middleX;
    else
	imageX = img_width - middleX;

    if (middleY > img_height - middleY)
	imageY = middleY;
    else
	imageY = img_height - middleY;
    
    imageR = hypot(imageX, imageY);

    if (!generate_code(argv[optind]))
    {
	printf("%s\n", error_string);
	exit(1);
    }

    output = (guchar*)malloc(outputBPP * img_width * img_height);
    assert(output != 0);

#ifdef MOVIES
    if (generate_movie)
    {
	output_movie = quicktime_open(argv[optind + 1], 0, 1);
	assert(output_movie != 0);

	quicktime_set_video(output_movie, 1, img_width, img_height, 25, QUICKTIME_JPEG);
	assert(quicktime_supported_video(output_movie, 0));
	quicktime_seek_start(output_movie);

	rows = (guchar**)malloc(sizeof(guchar*) * img_height);
	for (i = 0; i < img_height; ++i)
	    rows[i] = output + img_width * outputBPP * i;
    }
#endif

    for (current_frame = 0; current_frame < num_frames; ++current_frame)
    {
	currentT = (float)current_frame / (float)num_frames;

	update_image_internals();

	dest = output;

	if (oversamplingEnabled)
	{
	    guchar *line1, *line2, *line3;

	    line1 = (guchar*)malloc((img_width + 1) * outputBPP);
	    line2 = (guchar*)malloc(img_width * outputBPP);
	    line3 = (guchar*)malloc((img_width + 1) * outputBPP);

	    for (col = 0; col <= img_width; ++col)
	    {
		currentX = col - middleX;
		currentY = middleY;
		calc_ra();
		update_pixel_internals();
		write_tuple_to_pixel(EVAL_EXPR(), line1 + col * outputBPP);
	    }

	    for (row = 0; row < img_height; ++row)
	    {
		for (col = 0; col < img_width; ++col)
		{
		    currentX = col + 0.5 - middleX;
		    currentY = -(row + 0.5 - middleY);
		    calc_ra();
		    update_pixel_internals();
		    write_tuple_to_pixel(EVAL_EXPR(), line2 + col * outputBPP);
		}
		for (col = 0; col <= img_width; ++col)
		{
		    currentX = col - middleX;
		    currentY = -(row + 1.0 - middleY);
		    calc_ra();
		    update_pixel_internals();
		    write_tuple_to_pixel(EVAL_EXPR(), line3 + col * outputBPP);
		}
	    
		for (col = 0; col < img_width; ++col)
		{
		    int i;

		    for (i = 0; i < outputBPP; ++i)
			dest[i] = (line1[col*outputBPP+i]
				   + line1[(col+1)*outputBPP+i]
				   + 2*line2[col*outputBPP+i]
				   + line3[col*outputBPP+i]
				   + line3[(col+1)*outputBPP+i]) / 6;
		    dest += outputBPP;
		}

		memcpy(line1, line3, (img_width + 1) * outputBPP);
		++current_time;
	    }
	}
	else
	{
	    for (row = 0; row < img_height; row++)
	    {
		for (col = 0; col < img_width; col++)
		{
		    currentX = col - middleX;
		    currentY = -(row - middleY);
		    calc_ra();
		    update_pixel_internals();
		    write_tuple_to_pixel(EVAL_EXPR(), dest);
		    dest += outputBPP;
		}
		++current_time;
	    }
	}

#ifdef MOVIES
	if (generate_movie)
	{
	    fprintf(stderr, "writing frame %d\n", current_frame);
	    assert(quicktime_encode_video(output_movie, rows, 0) == 0);
	}
#endif
    }

#ifdef MOVIES
    if (generate_movie)
	quicktime_close(output_movie);
    else
#endif MOVIES
	write_image(argv[optind + 1], img_width, img_height, output, IMAGE_FORMAT_PNG);

    return 0;
}
