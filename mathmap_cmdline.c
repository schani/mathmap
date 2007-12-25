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
#include <glib.h>

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
#include "drawable.h"
#include "rwimg/readimage.h"
#include "rwimg/writeimage.h"

#include "generators/blender/blender.h"
//#include "generators/pixeltree/pixeltree.h"

typedef struct _cache_entry_t
{
    input_drawable_t *drawable;
    int frame;
    guchar *data;
    int timestamp;
} cache_entry_t;

static int cache_size = 16;
static cache_entry_t *cache = 0;
static int current_time = 0;

static cache_entry_t*
get_free_cache_entry (void)
{
    int lru_index = -1;
    int i;

    if (cache == 0)
	cache = g_new0(cache_entry_t, cache_size);
    g_assert(cache != 0);

    for (i = 0; i < cache_size; ++i)
	if (cache[i].drawable == 0)
	{
	    lru_index = i;
	    break;
	}
	else
	    if (lru_index < 0 || cache[i].timestamp < cache[lru_index].timestamp)
		lru_index = i;

    if (cache[lru_index].drawable != 0)
	cache[lru_index].drawable->v.cmdline.cache_entries[cache[lru_index].frame] = 0;

    if (cache[lru_index].data != 0)
    {
	free(cache[lru_index].data);
	cache[lru_index].data = 0;
    }

    return &cache[lru_index];
}

static cache_entry_t*
get_cache_entry_for_image (const char *filename, int *width, int *height)
{
    cache_entry_t *cache_entry = get_free_cache_entry();

    cache_entry->data = read_image(filename, width, height);
    if (cache_entry->data == 0)
    {
	fprintf(stderr, "Error: Cannot read input image `%s'.\n", filename);
	exit(1);
    }

    return cache_entry;
}

static void
bind_cache_entry_to_drawable (cache_entry_t *cache_entry, input_drawable_t *drawable, int frame)
{
    g_assert(drawable->kind == INPUT_DRAWABLE_CMDLINE_IMAGE
	     || drawable->kind == INPUT_DRAWABLE_CMDLINE_MOVIE);

    cache_entry->drawable = drawable;
    cache_entry->frame = frame;
    cache_entry->timestamp = current_time;

    drawable->v.cmdline.cache_entries[frame] = cache_entry;
}

color_t
cmdline_mathmap_get_pixel (mathmap_invocation_t *invocation, input_drawable_t *drawable, int frame, int x, int y)
{
    guchar *p;
    int num_frames;
    cache_entry_t **cache_entries;

    g_assert(drawable->kind == INPUT_DRAWABLE_CMDLINE_IMAGE || drawable->kind == INPUT_DRAWABLE_CMDLINE_MOVIE);

    num_frames = drawable->v.cmdline.num_frames;
    cache_entries = drawable->v.cmdline.cache_entries;

    if (frame < 0 || frame >= num_frames)
	return MAKE_RGBA_COLOR(255, 255, 255, 255);

    if (cache_entries[frame] == 0)
    {
	cache_entry_t *cache_entry;

	if (drawable->kind == INPUT_DRAWABLE_CMDLINE_IMAGE)
	{
	    int width, height;

	    cache_entry = get_cache_entry_for_image(drawable->v.cmdline.image_filename, &width, &height);

	    g_assert(width == drawable->width && height == drawable->height);
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

	bind_cache_entry_to_drawable(cache_entry, drawable, frame);
    }
    else
	cache_entries[frame]->timestamp = current_time;

    p = cache_entries[frame]->data + 3 * (drawable->width * y + x);

    return MAKE_RGBA_COLOR(p[0], p[1], p[2], 255);
}

input_drawable_t*
alloc_cmdline_image_input_drawable (const char *filename)
{
    int width, height;
    cache_entry_t *cache_entry = get_cache_entry_for_image(filename, &width, &height);
    input_drawable_t *drawable = alloc_input_drawable(INPUT_DRAWABLE_CMDLINE_IMAGE, width, height);

    drawable->v.cmdline.cache_entries = g_new0(cache_entry_t*, 1);
    drawable->v.cmdline.num_frames = 1;
    drawable->v.cmdline.image_filename = strdup(filename);

    bind_cache_entry_to_drawable(cache_entry, drawable, 0);

    return drawable;
}

#ifdef MOVIES
input_drawable_t*
alloc_cmdline_movie_input_drawable (const char *filename)
{
    g_assert_not_reached();

    /*
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
    */
}
#endif

static int
parse_image_size (char *str, int *width, int *height)
{
    char *x, *endptr;

    str = strdup(str);
    assert(str != 0);

    x = strchr(str, 'x');
    if (x == 0)
	return 0;

    *(x++) = '\0';

    if (strlen(str) <= 0 || strlen(x) <= 0)
	return 0;

    *width = strtol(str, &endptr, 10);
    if (*endptr != '\0')
	return 0;

    *height = strtol(x, &endptr, 10);
    if (*endptr != '\0')
	return 0;

    free(str);

    return 1;
}

static void
usage (void)
{
    printf("Usage:\n"
	   "  mathmap --version\n"
	   "      print out version number\n"
	   "  mathmap --help\n"
	   "      print this help text\n"
	   "  mathmap [option ...] [<script>] <outfile>\n"
	   "      transform one or more inputs with <script> and write\n"
	   "      the result to <outfile>\n"
	   "Options:\n"
	   "  -f, --script-file=FILENAME  read script from FILENAME\n"
	   "  -I, --image=FILENAME        input image FILENAME\n"
#ifdef MOVIES
	   "  -M, --movie=FILENAME        input movie FILENAME\n"
	   "  -F, --frames=NUM            output movie has NUM frames\n"
#endif
	   "  -i, --intersampling         use intersampling\n"
	   "  -o, --oversampling          use oversampling\n"
	   "  -s, --size=WIDTHxHEIGHT     sets the output image size\n"
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
    int num_frames = 1;
#ifdef MOVIES
    int generate_movie = 0;
    quicktime_t *output_movie;
    guchar **rows;
#endif
    int antialiasing = 0, supersampling = 0;
    int img_width, img_height;
    char *generator = 0;
    char *template_filename;
    userval_info_t *userval_info;
    int drawable_index;
    int size_is_set = 0;
    char *script = NULL;
    char *output_filename;

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
		{ "size", required_argument, 0, 's' },
		{ "script-file", required_argument, 0, 'f' },
#ifdef MOVIES
		{ "frames", required_argument, 0, 'F' },
		{ "movie", required_argument, 0, 'M' },
#endif
		{ 0, 0, 0, 0 }
	    };

	int option, option_index;

	option = getopt_long(argc, argv, 
#ifdef MOVIES
			     "f:ioF:I:M:c:g:s:", 
#else
			     "f:ioI:c:g:s:",
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

	    case 'f' :
		if (!g_file_get_contents(optarg, &script, NULL, NULL))
		{
		    fprintf(stderr, "Error: The script file `%s' could not be read.\n", optarg);
		    return 1;
		}
		break;

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
		alloc_cmdline_image_input_drawable(optarg);
		break;

	    case 'g' :
		generator = optarg;
		break;

	    case 's' :
		if (!parse_image_size(optarg, &img_width, &img_height))
		{
		    fprintf(stderr, "Error: Invalid image size.  Syntax is <width>x<height>.  Example: 1024x768.\n");
		    exit(1);
		}
		size_is_set = 1;
		break;

#ifdef MOVIES
	    case 'F' :
		generate_movie = 1;
		num_frames = atoi(optarg);
		assert(num_frames > 0);
		break;

	    case 'M' :
		alloc_cmdline_movie_input_drawable(optarg);
		break;
#endif
	}
    }

    if (script != NULL)
    {
	if (argc - optind != 1)
	{
	    usage();
	    return 1;
	}

	output_filename = argv[optind];
    }
    else
    {
	if (argc - optind != 2)
	{
	    usage();
	    return 1;
	}

	script = argv[optind];
	output_filename = argv[optind + 1];
    }

    init_tags();
    init_builtins();
    init_macros();
    init_noise();
    init_compiler();

    if (generator == 0)
    {
	int num_input_drawables = get_num_input_drawables();
	mathmap_t *mathmap;
	mathmap_invocation_t *invocation;

	if (!size_is_set)
	{
	    input_drawable_t *drawable;

	    if (num_input_drawables == 0)
	    {
		fprintf(stderr, "Error: Image size not set and no input images given.\n");
		exit(1);
	    }

	    drawable = get_nth_input_drawable(0);

	    if (drawable->kind == INPUT_DRAWABLE_CMDLINE_IMAGE)
	    {
		cache_entry_t *cache_entry = get_cache_entry_for_image(drawable->v.cmdline.image_filename,
								       &img_width, &img_height);

		bind_cache_entry_to_drawable(cache_entry, drawable, 0);
	    }
#ifdef MOVIES
	    else
	    {
		img_width = quicktime_video_width(input_drawables[0].v.movie, 0);
		img_height = quicktime_video_height(input_drawables[0].v.movie, 0);
	    }
#endif
	}

#ifdef MOVIES
	for (i = 0; i < num_input_drawables; ++i)
	    if (input_drawables[i].type == DRAWABLE_MOVIE)
	    {
		assert(quicktime_video_width(input_drawables[i].v.movie, 0) == img_width);
		assert(quicktime_video_height(input_drawables[i].v.movie, 0) == img_height);
	    }
#endif

	template_filename = g_strdup_printf("%s/mathmap/%s", GIMPDATADIR, MAIN_TEMPLATE_FILENAME);

	mathmap = compile_mathmap(script,
				  GIMPDATADIR "/mathmap/" MAIN_TEMPLATE_FILENAME,
				  GIMPDATADIR "/mathmap");
	if (mathmap == 0)
	{
	    fprintf(stderr, "Error: %s\n", error_string);
	    exit(1);
	}

	invocation = invoke_mathmap(mathmap, 0, img_width, img_height);

	drawable_index = 0;
	for (userval_info = mathmap->main_filter->userval_infos; userval_info != 0; userval_info = userval_info->next)
	    if (userval_info->type == USERVAL_IMAGE)
	    {
		userval_t *userval = &invocation->uservals[userval_info->index];

		if (drawable_index >= num_input_drawables)
		{
		    fprintf(stderr, "Error: Not enough input images specified.\n");
		    exit(1);
		}

		assign_image_userval_drawable(userval_info, userval, get_nth_input_drawable(drawable_index));

		++drawable_index;
	    }

	if (drawable_index != num_input_drawables)
	{
	    fprintf(stderr, "Error: Too many input images specified.\n");
	    exit(1);
	}

	invocation->antialiasing = antialiasing;
	invocation->supersampling = supersampling;

	invocation->output_bpp = 4;

	output = (guchar*)malloc(invocation->output_bpp * img_width * img_height);
	assert(output != 0);

#ifdef MOVIES
	if (generate_movie)
	{
	    output_movie = quicktime_open(output_filename, 0, 1);
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

	    call_invocation_parallel_and_join(invocation, 0, 0, img_width, img_height, output, 1);

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
	    write_image(output_filename, img_width, img_height, output,
			invocation->output_bpp, img_width * invocation->output_bpp, IMAGE_FORMAT_PNG);
    }
    else
    {
	if (strcmp(generator, "blender") == 0)
	{
	    if (!blender_generate_plug_in(script, output_filename))
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
