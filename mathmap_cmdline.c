/* -*- c -*- */

/*
 * mathmap_cmdline.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2007 Mark Probst
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
#include "tags.h"
#include "scanner.h"
#include "vars.h"
#include "userval.h"
#include "internals.h"
#include "macros.h"
#include "jump.h"
#include "mathmap.h"
#include "noise.h"
#include "rwimg/readimage.h"
#include "rwimg/writeimage.h"

#include "generators/blender/blender.h"
//#include "generators/pixeltree/pixeltree.h"

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

mathmap_t *mathmap;
mathmap_invocation_t *invocation;

color_t
cmdline_mathmap_get_pixel (mathmap_invocation_t *invocation, int drawable_index, int frame, int x, int y)
{
    guchar *p;
    int i;
    input_drawable_t *drawable;

    if (drawable_index < 0 || drawable_index >= num_input_drawables
	|| x < 0 || x >= invocation->calc_img_width
	|| y < 0 || y >= invocation->calc_img_height
	|| frame < 0 || frame >= input_drawables[drawable_index].num_frames)
	return invocation->edge_color;

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
	    assert(width == invocation->calc_img_width && height == invocation->calc_img_height);
	}
#ifdef MOVIES
	else
	{
	    guchar **rows = (guchar**)malloc(sizeof(guchar*) * invocation->img_height);

	    if (cache[lru_index].data == 0)
		cache[lru_index].data = (guchar*)malloc(invocation->calc_img_width * invocation->calc_img_height * 3);

	    for (i = 0; i < invocation->calc_img_height; ++i)
		rows[i] = cache[lru_index].data + i * invocation->calc_img_width * 3;

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

    p = drawable->cache_entries[frame]->data + 3 * (invocation->calc_img_width * y + x);

    return MAKE_RGBA_COLOR(p[0], p[1], p[2], 255);
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
	   "  -g, --generator=GEN         generate plug-in code with GEN\n"
	   "\n"
	   "Report bugs and suggestions to schani@complang.tuwien.ac.at\n",
	   cache_size);
}

int
cmdline_main (int argc, char *argv[])
{
    guchar *output;
    int i;
    int num_frames = 1;
#ifdef MOVIES
    int generate_movie = 0;
    quicktime_t *output_movie;
    guchar **rows;
#endif
    int antialiasing = 0, supersampling = 0;
    int img_width, img_height;
    FILE *template;
    char *generator = 0;

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
		{ "generator", required_argument, 0, 'g' },
#ifdef MOVIES
		{ "frames", required_argument, 0, 'f' },
		{ "movie", required_argument, 0, 'M' },
#endif
		{ 0, 0, 0, 0 }
	    };

	int option, option_index;

	option = getopt_long(argc, argv, 
#ifdef MOVIES
			     "iof:I:M:c:g:", 
#else
			     "ioI:c:g:",
#endif
			     long_options, &option_index);

	if (option == -1)
	    break;

	switch (option)
	{
	    case 256 :
		printf("MathMap " MATHMAP_VERSION "\n"
		       "\n"
		       "Copyright (C) 1997-2007 Mark Probst\n"
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
		antialiasing = 1;
		break;

	    case 'o' :
		supersampling = 1;
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

	    case 'g' :
		generator = optarg;
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

    init_tags();
    init_builtins();
    init_macros();
    init_noise();
    init_compiler();

    if (generator == 0)
    {
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
	    if (cache[0].data == 0)
	    {
		fprintf(stderr, "Error: could not load image `%s'.", input_drawables[0].v.image_filename);
		exit(1);
	    }
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

	template = fopen("new_template.c", "r");
	if (template == 0)
	{
	    fprintf(stderr, "Error: could not open template file new_template.c.\n");
	    exit(1);
	}
	mathmap = compile_mathmap(argv[optind], template, "opmacros.h");
	if (mathmap == 0)
	{
	    fprintf(stderr, "Error: %s\n", error_string);
	    exit(1);
	}

	invocation = invoke_mathmap(mathmap, 0, img_width, img_height);

	invocation->antialiasing = antialiasing;
	invocation->supersampling = supersampling;

	invocation->output_bpp = 4;

	output = (guchar*)malloc(invocation->output_bpp * img_width * img_height);
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
		rows[i] = output + img_width * invocation->output_bpp * i;
	}
#endif

	for (invocation->current_frame = 0; invocation->current_frame < num_frames; ++invocation->current_frame)
	{
	    invocation->current_t = (float)invocation->current_frame / (float)num_frames;

	    update_image_internals(invocation);

	    call_invocation(invocation, 0, img_height, output);

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
#endif
	    write_image(argv[optind + 1], img_width, img_height, output,
			invocation->output_bpp, img_width * invocation->output_bpp, IMAGE_FORMAT_PNG);
    }
    else
    {
	if (strcmp(generator, "blender") == 0)
	{
	    if (!blender_generate_plug_in(argv[optind], argv[optind + 1]))
		return 1;
	}
	/*
	else if (strcmp(generator, "pixeltree") == 0)
	{
	    if (!pixeltree_generate_plug_in(argv[optind], argv[optind + 1]))
		return 1;
	}
	*/
	else
	{
	    fprintf(stderr, "Unknown generator `%s'\n", generator);
	    return 1;
	}
    }

    return 0;
}
