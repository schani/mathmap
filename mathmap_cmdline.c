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
#include <ctype.h>
#include <errno.h>

#include <glib.h>

#include "getopt.h"

#include "exprtree.h"
#include "builtins/builtins.h"
#include "tags.h"
#include "scanner.h"
#include "vars.h"
#include "userval.h"
#include "internals.h"
#include "macros.h"
#include "jump.h"
#include "mathmap.h"
#include "drawable.h"
#include "rwimg/readimage.h"
#include "rwimg/writeimage.h"

#include "generators/blender/blender.h"
#include "generators/nacl/nacl.h"

typedef struct _define_t
{
    char *name;
    char *value;
    struct _define_t *next;
} define_t;

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

static long num_pixels_requested = 0;

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
	fprintf(stderr, _("Error: Cannot read input image `%s'.\n"), filename);
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
mathmap_get_pixel (mathmap_invocation_t *invocation, input_drawable_t *drawable, int frame, int x, int y)
{
    guchar *p;
    int num_frames;
    cache_entry_t **cache_entries;

    g_assert (drawable != NULL);
    g_assert (drawable->kind == INPUT_DRAWABLE_CMDLINE_IMAGE || drawable->kind == INPUT_DRAWABLE_CMDLINE_MOVIE);

    ++num_pixels_requested;

    if (x < 0 || x >= drawable->image.pixel_width)
	return invocation->edge_color_x;
    if (y < 0 || y >= drawable->image.pixel_height)
	return invocation->edge_color_y;

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

	    g_assert(width == drawable->image.pixel_width && height == drawable->image.pixel_height);
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

    p = cache_entries[frame]->data + 3 * (drawable->image.pixel_width * y + x);

    return MAKE_RGBA_COLOR(p[0], p[1], p[2], 255);
}

void
drawable_get_pixel_inc (mathmap_invocation_t *invocation, input_drawable_t *drawable, int *inc_x, int *inc_y)
{
    *inc_x = *inc_y = 1;
}

gradient_t*
get_default_gradient (void)
{
    /* FIXME: implement */
    return NULL;
}

input_drawable_t*
get_default_input_drawable (void)
{
    /* FIXME: implement */
    return NULL;
}

void
mathmap_message_dialog (const char *message)
{
    printf ("%s\n", message);
}

void
save_debug_tuples (mathmap_invocation_t *invocation, int row, int col)
{
    /*
    pixel_debug_info_t *info = get_pixel_debug_info(row, col);
    int i;

    info->num_debug_tuples = invocation->num_debug_tuples;
    assert(info->num_debug_tuples <= MAX_DEBUG_TUPLES);

    for (i = 0; i < info->num_debug_tuples; ++i)
	info->debug_tuples[i] = invocation->debug_tuples[i];
    */
}

/*
static void
print_tuple (tuple_t *tuple)
{
    const char *name = tag_name_for_number(tuple->number);
    int i;

    assert(name != 0);

    printf("%s:[", name);
    for (i = 0; i < tuple->length; ++i)
    {
	gchar buf[G_ASCII_DTOSTR_BUF_SIZE];

	g_ascii_dtostr(buf, sizeof(buf), tuple->data[i]);
	fputs(buf, stdout);
	if (i + 1 < tuple->length)
	    printf(",");
    }
    printf("]");
}
*/

void
delete_expression_marker (void)
{
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

static gboolean
is_ident_char (char c)
{
    if (isalnum(c))
	return TRUE;
    return c == '_';
}

static gboolean
print_html_char (FILE *out, char c)
{
    /* FIXME: Implement properly */
    if (putc(c, out) == EOF)
	return FALSE;
    return TRUE;
}

static gboolean
write_filter_html_doc (FILE *out, top_level_decl_t *decl)
{
    arg_decl_t *arg;
    int num_args;
    int arg_indent;

    g_assert(decl->type == TOP_LEVEL_FILTER);

    fprintf(out, "<p><a name=\"filter_%s\"></a><font size=\"+1\"><tt>filter <b>%s</b> (",
	    decl->name, decl->name);

    num_args = 0;
    for (arg = decl->v.filter.args; arg != NULL; arg = arg->next)
	++num_args;

    arg_indent = strlen("filter") + strlen(decl->name) + 3;

    for (arg = decl->v.filter.args; arg != NULL; arg = arg->next)
    {
	static struct { int num; const char *name; } types[] =
	    { { ARG_TYPE_INT, "int" },
	      { ARG_TYPE_FLOAT, "float" },
	      { ARG_TYPE_COLOR, "color" },
	      { ARG_TYPE_GRADIENT, "gradient" },
	      { ARG_TYPE_CURVE, "curve" },
	      { ARG_TYPE_IMAGE, "image" },
	      { 0, NULL } };

	int i;

	for (i = 0; types[i].name != NULL; ++i)
	    if (types[i].num == arg->type)
		break;
	g_assert(types[i].name != NULL);

	fprintf(out, "%s <b>%s</b>", types[i].name, arg->name);

	switch (arg->type)
	{
	    case ARG_TYPE_INT :
		if (arg->v.integer.have_limits)
		    fprintf(out, ": %d - %d", arg->v.integer.min, arg->v.integer.max);
		break;

	    case ARG_TYPE_FLOAT :
		if (arg->v.floating.have_limits)
		    fprintf(out, ": %g - %g", arg->v.floating.min, arg->v.floating.max);
		break;

	    default :
		break;
	}

	if (arg->next != NULL)
	{
	    if (num_args > 2)
	    {
		int i;

		fprintf(out, ",\n<br>");

		for (i = 0; i < arg_indent; ++i)
		    fprintf(out, "&nbsp;");
	    }
	    else
		fprintf(out, ", ");
	}
    }

    fprintf(out, ")</font>\n");

    if (decl->docstring != NULL)
    {
	char *p;
	gboolean bold_mode = FALSE;
	gboolean newline = FALSE;

	fprintf(out, "<blockquote>");
	for (p = decl->docstring; *p != '\0'; ++p)
	{
	    if (*p == '\n')
	    {
		if (newline)
		{
		    fprintf(out, "\n<p>");
		    continue;
		}
		newline = TRUE;
	    }
	    else
		newline = FALSE;

	    if (*p == '@')
	    {
		fprintf(out, "<b>");
		bold_mode = TRUE;
	    }
	    else
	    {
		if (bold_mode && !is_ident_char(*p))
		{
		    fprintf(out, "</b>");
		    bold_mode = FALSE;
		}

		if (!print_html_char(out, *p))
		    return FALSE;
	    }
	}
	fprintf(out, "</blockquote>\n");
    }

    return TRUE;
}

static void
append_define (const char *str, define_t **defines)
{
    define_t *define = g_new0(define_t, 1);
    const char *p;

    p = strchr(str, '=');
    if (p == NULL)
    {
	fprintf(stderr, _("Error: Option to -D is malformed: `%s'.\n"), str);
	exit(1);
    }

    define->name = malloc(p - str + 1);
    memcpy(define->name, str, p - str);
    define->name[p - str] = '\0';

    define->value = strdup(p + 1);

    define->next = *defines;
    *defines = define;
}

static define_t*
lookup_define (define_t *defines, const char *name)
{
    while (defines != NULL)
    {
	if (strcmp(name, defines->name) == 0)
	    return defines;

	defines = defines->next;
    }

    return NULL;
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
	   "  mathmap --htmldoc [<script>] <outfile>\n"
	   "      outputs HTML documentation for the filters in\n"
	   "      the script to <outfile>\n"
	   "Options:\n"
	   "  -f, --script-file=FILENAME  read script from FILENAME\n"
	   "  -D<name>=<value>            define user value\n"
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

#define OPTION_VERSION				256
#define OPTION_HELP				257
#define OPTION_HTMLDOC				258
#define OPTION_BENCH_NO_OUTPUT			259
#define OPTION_BENCH_ONLY_COMPILE		260
#define OPTION_BENCH_NO_COMPILE_TIME_LIMIT	261
#define OPTION_BENCH_NO_BACKEND			262
#define OPTION_BENCH_RENDER_COUNT		263

int
main (int argc, char *argv[])
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
    userval_info_t *userval_info;
    int num_input_drawables = 0;
    gboolean size_is_set = FALSE;
    char *script = NULL;
    char *output_filename;
    gboolean htmldoc = FALSE;
    define_t *defines = NULL;
    int bench_render_count = 1;
    int render_num;
    gboolean bench_no_output = FALSE;
    gboolean bench_no_backend = FALSE;
    int compile_time_limit = DEFAULT_OPTIMIZATION_TIMEOUT;

    init_gettext();

    for (;;)
    {
	static struct option long_options[] =
	    {
		{ "version", no_argument, 0, OPTION_VERSION },
		{ "help", no_argument, 0, OPTION_HELP },
		{ "intersampling", no_argument, 0, 'i' },
		{ "oversampling", no_argument, 0, 'o' },
		{ "cache", required_argument, 0, 'c' },
		{ "generator", required_argument, 0, 'g' },
		{ "size", required_argument, 0, 's' },
		{ "script-file", required_argument, 0, 'f' },
		{ "htmldoc", no_argument, 0, OPTION_HTMLDOC },
		{ "bench-no-output", no_argument, 0, OPTION_BENCH_NO_OUTPUT },
		{ "bench-only-compile", no_argument, 0, OPTION_BENCH_ONLY_COMPILE },
		{ "bench-no-compile-time-limit", no_argument, 0, OPTION_BENCH_NO_COMPILE_TIME_LIMIT },
		{ "bench-no-backend", no_argument, 0, OPTION_BENCH_NO_BACKEND },
		{ "bench-render-count", required_argument, 0, OPTION_BENCH_RENDER_COUNT },
#ifdef MOVIES
		{ "frames", required_argument, 0, 'F' },
		{ "movie", required_argument, 0, 'M' },
#endif
		{ 0, 0, 0, 0 }
	    };

	int option, option_index;

	option = getopt_long(argc, argv, 
#ifdef MOVIES
			     "f:ioF:D:M:c:g:s:", 
#else
			     "f:ioD:c:g:s:",
#endif
			     long_options, &option_index);

	if (option == -1)
	    break;

	switch (option)
	{
	    case OPTION_VERSION :
		printf("MathMap " MATHMAP_VERSION "\n"
		       "\n"
		       "Copyright (C) 1997-2009 Mark Probst\n"
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

	    case OPTION_HELP :
		usage();
		return 0;

	    case OPTION_HTMLDOC :
		htmldoc = TRUE;
		break;

	    case 'f' :
		if (!g_file_get_contents(optarg, &script, NULL, NULL))
		{
		    fprintf(stderr, _("Error: The script file `%s' could not be read.\n"), optarg);
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

	    case 'D' :
		append_define(optarg, &defines);
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
		    fprintf(stderr, _("Error: Invalid image size.  Syntax is <width>x<height>.  Example: 1024x768.\n"));
		    exit(1);
		}
		size_is_set = 1;
		break;

	    case OPTION_BENCH_RENDER_COUNT :
		bench_render_count = atoi(optarg);
		break;

	    case OPTION_BENCH_ONLY_COMPILE :
		bench_render_count = 0;
		break;

	    case OPTION_BENCH_NO_OUTPUT :
		bench_no_output = TRUE;
		break;

	    case OPTION_BENCH_NO_COMPILE_TIME_LIMIT :
		compile_time_limit = -1;
		break;

	    case OPTION_BENCH_NO_BACKEND :
		bench_no_backend = TRUE;
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
    init_compiler();

    if (htmldoc)
    {
	mathmap_t *mathmap = parse_mathmap(script);
	filter_t *filter;
	FILE *out;

	if (mathmap == NULL)
	{
	    fprintf(stderr, _("Error: Could not read MathMap script: %s\n"), error_string);
	    return 1;
	}

	out = fopen(output_filename, "w");
	if (out == NULL)
	{
	    fprintf(stderr, _("Error: Cannot open file `%s' for writing: %s\n"),
		    output_filename, strerror(errno));
	    return 1;
	}

	for (filter = mathmap->filters; filter != NULL; filter = filter->next)
	{
	    if (filter->kind != FILTER_MATHMAP)
		continue;

	    if (!write_filter_html_doc(out, filter->v.mathmap.decl))
		return 1;
	}

	fclose(out);
    }
    else if (generator == 0)
    {
	char *support_paths[4];
	mathmap_t *mathmap;
	mathmap_invocation_t *invocation;
	int current_frame;
	int i = 0;

	/*
	support_paths[i++] = g_strdup_printf("%s/mathmap", GIMPDATADIR);
	support_paths[i++] = g_strdup_printf("%s/.gimp-2.6/mathmap", getenv("HOME"));
	support_paths[i++] = g_strdup_printf("%s/.gimp-2.4/mathmap", getenv("HOME"));
	*/
	support_paths[i] = NULL;

	mathmap = compile_mathmap(script, support_paths, compile_time_limit, bench_no_backend);

	if (bench_no_backend)
	    return 0;

	if (mathmap == 0)
	{
	    fprintf(stderr, _("Error: %s\n"), error_string);
	    exit(1);
	}

	if (bench_render_count == 0)
	    return 0;

	if (!size_is_set)
	    for (userval_info = mathmap->main_filter->userval_infos;
		 userval_info != NULL;
		 userval_info = userval_info->next)
	    {
		define_t *define;
		unsigned char *image;

		if (userval_info->type != USERVAL_IMAGE)
		    continue;

		define = lookup_define(defines, userval_info->name);
		if (define == NULL)
		{
		    fprintf(stderr, _("Error: No value defined for input image `%s'.\n"), userval_info->name);
		    return 1;
		}

		image = read_image(define->value, &img_width, &img_height);
		if (image == NULL)
		{
		    fprintf(stderr, _("Error: Could not read input image `%s'.\n"), define->value);
		    return 1;
		}
		free(image);

		size_is_set = TRUE;

		break;
	    }

	if (!size_is_set)
	{
	    fprintf(stderr, _("Error: Image size not set and no input images given.\n"));
	    exit(1);
	}

	invocation = invoke_mathmap(mathmap, NULL, img_width, img_height, TRUE);

	for (userval_info = mathmap->main_filter->userval_infos;
	     userval_info != NULL;
	     userval_info = userval_info->next)
	{
	    userval_t *userval = &invocation->uservals[userval_info->index];
	    define_t *define = lookup_define(defines, userval_info->name);

	    if (define == NULL)
	    {
		if (userval_info->type == USERVAL_IMAGE)
		{
		    fprintf(stderr, _("Error: No value defined for input image `%s'.\n"), userval_info->name);
		    return 1;
		}
	    }
	    else
		switch (userval_info->type)
		{
		    case USERVAL_INT_CONST :
			userval->v.int_const = atoi(define->value);
			break;

		    case USERVAL_FLOAT_CONST :
			userval->v.float_const = g_ascii_strtod(define->value, NULL);
			break;

		    case USERVAL_BOOL_CONST :
			userval->v.bool_const = (float)atoi(define->value);
			break;

		    case USERVAL_IMAGE :
			assign_image_userval_drawable(userval_info, userval,
						      alloc_cmdline_image_input_drawable(define->value));
			++num_input_drawables;
			break;

		    default :
			fprintf(stderr, _("Error: Can only define user values for types int, float, bool and image.\n"));
			return 1;
		}
	}

	for (render_num = 0; render_num < bench_render_count; ++render_num)
	{
#ifdef MOVIES
	    for (i = 0; i < num_input_drawables; ++i)
		if (input_drawables[i].type == DRAWABLE_MOVIE)
		{
		    assert(quicktime_video_width(input_drawables[i].v.movie, 0) == img_width);
		    assert(quicktime_video_height(input_drawables[i].v.movie, 0) == img_height);
		}
#endif

	    invocation_set_antialiasing(invocation, antialiasing);
	    invocation->supersampling = supersampling;

	    invocation->output_bpp = 4;

	    output = (guchar*)malloc((long)invocation->output_bpp * (long)img_width * (long)img_height);
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

	    for (current_frame = 0; current_frame < num_frames; ++current_frame)
	    {
		float current_t = (float)current_frame / (float)num_frames;
		image_t *closure = closure_image_alloc(&invocation->mathfuncs,
						       NULL,
						       invocation->mathmap->main_filter->num_uservals,
						       invocation->uservals,
						       img_width, img_height);
		mathmap_frame_t *frame = invocation_new_frame(invocation, closure,
							      current_frame, current_t);

		call_invocation_parallel_and_join(frame, closure, 0, 0, img_width, img_height, output, 1);

		invocation_free_frame(frame);

#ifdef MOVIES
		if (generate_movie && !bench_no_output)
		{
		    fprintf(stderr, _("writing frame %d\n"), current_frame);
		    assert(quicktime_encode_video(output_movie, rows, 0) == 0);
		}
#endif

		closure_image_free(closure);
	    }

	    if (!bench_no_output)
	    {
#ifdef MOVIES
		if (generate_movie)
		    quicktime_close(output_movie);
		else
#endif
		    write_image(output_filename, img_width, img_height, output,
				invocation->output_bpp, img_width * invocation->output_bpp, IMAGE_FORMAT_PNG);
	    }

	    free(output);
	}
    }
    else
    {
	if (strcmp(generator, "blender") == 0)
	{
	    if (!blender_generate_plug_in(script, output_filename))
		return 1;
	}
	else if (strcmp (generator, "nacl") == 0)
	{
	    if (!nacl_generate_plug_in (script, output_filename))
		return 1;
	}
	else
	{
	    fprintf(stderr, _("Unknown generator `%s'\n"), generator);
	    return 1;
	}
    }

    return 0;
}
