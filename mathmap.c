/* The GIMP -- an image manipulation program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * MathMap plug-in --- generate an image by means of a mathematical expression
 * Copyright (C) 1997-2004 Mark Probst
 * schani@complang.tuwien.ac.at
 *
 * Plug-In structure based on:
 *   Whirl plug-in --- distort an image into a whirlpool
 *   Copyright (C) 1997 Federico Mena Quintero
 *   federico@nuclecu.unam.mx
 *
 * Version 0.14
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

#define MATHMAP_MANUAL_URL    "http://www.complang.tuwien.ac.at/schani/mathmap/manual.html"

#include <sys/param.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

#include <gtk/gtk.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <libgimp/gimpintl.h>

#include "lispreader.h"
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
#include "cgen.h"

/***** Magic numbers *****/
#define PREVIEW_SIZE 192
#define SCALE_WIDTH  200
#define ENTRY_WIDTH  60

/* Even more stuff from Quartics plugins */
#define CHECK_SIZE  8
#define CHECK_DARK  ((int) (1.0 / 3.0 * 255))
#define CHECK_LIGHT ((int) (2.0 / 3.0 * 255))   


#define DEFAULT_EXPRESSION      "origVal(xy*xy:[cos(pi/2/Y*y+t*2*pi),1])"
#define DEFAULT_NUMBER_FRAMES   10

#define FLAG_ANTIALIASING       1
#define FLAG_SUPERSAMPLING      2
#define FLAG_ANIMATION          4
#define FLAG_PERIODIC           8

#define MAX_EXPRESSION_LENGTH   8192

/***** Types *****/

typedef struct {
    gint flags;
    gint frames;
    gfloat param_t;
    gchar expression[MAX_EXPRESSION_LENGTH];
} mathmap_vals_t;

typedef struct {
    GtkWidget *preview;
    guchar *wimage;
    gint run;
} mathmap_interface_t;

typedef struct {
    GimpDrawable *drawable;
    gint bpp;
    gint row;
    gint col;
    GimpTile *tile;
    color_t *fast_image_source;
    int used;
} input_drawable_t;

/***** Prototypes *****/

static void query (void);
#ifndef GIMP2
static void run (char *name,
		 int nparams,
		 GimpParam *param,
		 int *nreturn_vals,
		 GimpParam **return_vals);
#else
static void run (const gchar *name,
		 gint nparams,
		 const GimpParam *param,
		 gint *nreturn_vals,
 		 GimpParam **return_vals);
#endif


static void expression_copy (gchar *dest, const gchar *src);

static void do_mathmap (int frame_num, float t);
static gint32 mathmap_layer_copy (gint32 layerID);

extern int yyparse (void);

static void build_fast_image_source (input_drawable_t *drawable);

static void update_userval_table (void);

static void update_gradient (void);
static gint mathmap_dialog (int);
static void dialog_update_preview (void);
static void dialog_scale_update (GtkAdjustment *adjustment, gint *value);
static void dialog_t_update (GtkAdjustment *adjustment, gfloat *value);
#ifndef GIMP2
static void dialog_text_changed (void);
#else
static void dialog_text_changed (GtkTextBuffer * buffer, gpointer user_data);
#endif
static void dialog_text_update (void);
static void dialog_antialiasing_update (GtkWidget *widget, gpointer data);
static void dialog_supersampling_update (GtkWidget *widget, gpointer data);
static void dialog_auto_preview_update (GtkWidget *widget, gpointer data);
static void dialog_fast_preview_update (GtkWidget *widget, gpointer data);
static void dialog_edge_behaviour_update (GtkWidget *widget, gpointer data);
static void dialog_edge_color_changed (GtkWidget *color_well, gpointer data);
static void dialog_animation_update (GtkWidget *widget, gpointer data);
static void dialog_periodic_update (GtkWidget *widget, gpointer data);
static void dialog_preview_callback (GtkWidget *widget, gpointer data);
static void dialog_ok_callback (GtkWidget *widget, gpointer data);
static void dialog_help_callback (GtkWidget *widget, gpointer data);
static void dialog_about_callback (GtkWidget *widget, gpointer data);
#ifndef GIMP2
static void dialog_tree_changed (GtkTree *tree);
#else
static void dialog_tree_changed (GtkTreeSelection *tree, gpointer data);
#endif
#ifdef GIMP2
static void dialog_response (GtkWidget *widget, gint response_id, gpointer data);
#endif

/***** Variables *****/

GimpPlugInInfo PLUG_IN_INFO = {
	NULL,   /* init_proc */
	NULL,   /* quit_proc */
	query,  /* query_proc */
	run     /* run_proc */
}; /* PLUG_IN_INFO */


static mathmap_vals_t mmvals = {
	FLAG_ANTIALIASING | FLAG_PERIODIC, /* flags */
	DEFAULT_NUMBER_FRAMES,	/* frames */
	0.0,			/* t */
	DEFAULT_EXPRESSION	/* expression */
}; /* mmvals */

static mathmap_interface_t wint = {
	NULL,  /* preview */
	NULL,  /* wimage */
	FALSE  /* run */
}; /* wint */

#define MAX_INPUT_DRAWABLES 64

#ifndef GIMP2
static GimpRunModeType run_mode;
#else
static GimpRunMode run_mode;
#endif
static gint32 image_id;
static gint32 layer_id;
static input_drawable_t input_drawables[MAX_INPUT_DRAWABLES];
static GimpDrawable *output_drawable;

static gint tile_width, tile_height;
gint sel_x1, sel_y1, sel_x2, sel_y2;
gint sel_width, sel_height;
gint preview_width, preview_height;

static long num_pixels_requested = 0;

GtkWidget *expression_entry = 0,
    *frame_table,
    *t_table,
    *edge_color_well,
    *uservalues_scrolled_window,
    *uservalues_table;
GtkColorSelectionDialog *color_selection_dialog;

int img_width, img_height;
int previewing = 0, auto_preview = 1, fast_preview = 1;
int expression_changed = 1;
color_t gradient_samples[USER_GRADIENT_POINTS];
int output_bpp;
int edge_behaviour_mode = EDGE_BEHAVIOUR_COLOR;

#ifndef GIMP2
static guchar edge_color[4] = { 0, 0, 0, 0 };
#else
static GimpRGB edge_color = { 0.0, 0.0, 0.0, 0.0 };
#endif

mathmap_t *mathmap = 0;
mathmap_invocation_t *invocation = 0;

/***** Functions *****/

/*****/

static void
expression_copy (gchar *dest, const gchar *src)
{
    strncpy(dest, src, MAX_EXPRESSION_LENGTH);
    dest[MAX_EXPRESSION_LENGTH - 1] = 0;
}


/*****/

MAIN()


/*****/

static FILE*
open_rc_file (const char *name)
{
    FILE *file;
    gchar *mathmap_name = g_strconcat("mathmap", G_DIR_SEPARATOR_S, name, NULL);
    gchar *filename;

    assert(mathmap_name != 0);

    filename = gimp_personal_rc_file(mathmap_name);
    assert(filename != 0);
    file = fopen(filename, "r");
    g_free(filename);

    if (file == 0)
    {
	filename = g_strconcat(gimp_data_directory(), G_DIR_SEPARATOR_S, mathmap_name, NULL);
	assert(filename != 0);
	file = fopen(filename, "r");
	g_free(filename);
    }

    g_free(mathmap_name);

    return file;
}

/*****/

static lisp_object_t*
read_rc_file (void)
{
    static lisp_object_t *obj = 0;

    FILE *file;
    lisp_stream_t stream;

    if (obj != 0)
	return obj;

    file = open_rc_file("mathmaprc");

    obj = lisp_read(lisp_stream_init_file(&stream, file));
    fclose(file);

    return obj;
}

static void
register_lisp_obj (lisp_object_t *obj, char *symbol_prefix, char *menu_prefix)
{
    int symbol_prefix_len = strlen(symbol_prefix);
    int menu_prefix_len = strlen(menu_prefix);

    for (; lisp_type(obj) != LISP_TYPE_NIL; obj = lisp_cdr(obj))
    {
	lisp_object_t *vars[2];
	int is_group = 0;
	lisp_object_t *name_obj, *data;
	char *symbol, *menu;
	int i;
	int name_len;

	assert(lisp_type(obj) == LISP_TYPE_CONS);

	if (lisp_match_string("(group #?(string) . #?(list))", lisp_car(obj), vars))
	    is_group = 1;
	else if (lisp_match_string("(expression #?(string) #?(string))", lisp_car(obj), vars))
	    is_group = 0;
	else
	    assert(0);

	name_obj = vars[0];
	data = vars[1];

	name_len = strlen(lisp_string(name_obj));

	symbol = g_malloc(symbol_prefix_len + name_len + 2);
	strcpy(symbol, symbol_prefix);
	strcat(symbol, "_");
	strcat(symbol, lisp_string(name_obj));

	menu = g_malloc(menu_prefix_len + name_len + 2);
	strcpy(menu, menu_prefix);
	strcat(menu, "/");
	strcat(menu, lisp_string(name_obj));

	for (i = symbol_prefix_len + 1; i < symbol_prefix_len + 1 + name_len; ++i)
	    if (symbol[i] == ' ')
		symbol[i] = '_';
	    else
		symbol[i] = tolower(symbol[i]);

	if (is_group)
	    register_lisp_obj(data, symbol, menu);
	else
	{
	    static GimpParamDef args[] = {
		{ GIMP_PDB_INT32,      "run_mode",         "Interactive, non-interactive" },
		{ GIMP_PDB_IMAGE,      "image",            "Input image" },
		{ GIMP_PDB_DRAWABLE,   "drawable",         "Input drawable" },
		{ GIMP_PDB_INT32,      "flags",            "1: Antialiasing 2: Supersampling 4: Animate 8: Periodic" },
		{ GIMP_PDB_INT32,      "frames",           "Number of frames" },
		{ GIMP_PDB_FLOAT,      "param_t",          "The parameter t (if not animating)" },
		{ GIMP_PDB_STRING,     "expression",       "The expression" }
	    };
	    static GimpParamDef *return_vals  = NULL;
	    static int nargs = sizeof(args) / sizeof(args[0]);
	    static int nreturn_vals = 0;

	    fprintf(stderr, "registering %s (%s)\n", symbol, menu);

	    gimp_install_procedure(symbol,
				   "Generate an image using a mathematical expression.",
				   "Generates an image by means of a mathematical expression. The expression "
				   "can also refer to the data of an original image. Thus, arbitrary "
				   "distortions can be constructed. Even animations can be generated.",
				   "Mark Probst",
				   "Mark Probst",
				   MATHMAP_DATE ", " MATHMAP_VERSION,
				   menu,
				   "RGB*, GRAY*",
				   GIMP_PLUGIN,
				   nargs,
				   nreturn_vals,
				   args,
				   return_vals);
	}

	g_free(menu);
	g_free(symbol);
    }
}

static void
register_examples (void)
{
    lisp_object_t *obj = read_rc_file();

    if (obj == 0)
	return;

    register_lisp_obj(obj, "mathmap", "<Image>/Filters/Generic/MathMap");
    lisp_free(obj);
}

static char*
expression_for_symbol (const char *symbol, lisp_object_t *obj)
{
    for (; lisp_type(obj) != LISP_TYPE_NIL; obj = lisp_cdr(obj))
    {
	lisp_object_t *vars[2];
	int is_group = 0;
	char *name;
	int i;
	int name_len;

	assert(lisp_type(obj) == LISP_TYPE_CONS);

	if (lisp_match_string("(group #?(string) . #?(list))", lisp_car(obj), vars))
	    is_group = 1;
	else if (lisp_match_string("(expression #?(string) #?(string))", lisp_car(obj), vars))
	    is_group = 0;
	else
	    assert(0);

	name = lisp_string(vars[0]);
	name_len = strlen(name);

	if (name_len > strlen(symbol))
	    continue;
	if ((!is_group && name_len != strlen(symbol))
	    || (is_group && name_len == strlen(symbol)))
	    continue;
	if (is_group && symbol[name_len] != '_')
	    continue;

	for (i = 0; i < name_len; ++i)
	    if ((name[i] == ' ' && symbol[i] != '_')
		|| (name[i] != ' ' && symbol[i] != tolower(name[i])))
		break;

	if (i == name_len)
	{
	    if (is_group)
	    {
		char *exp = expression_for_symbol(symbol + name_len + 1, vars[1]);

		if (exp != 0)
		    return exp;
	    }
	    else
		return lisp_string(vars[1]);
	}
    }

    return 0;
}

static void
query(void)
{
    static GimpParamDef args[] = {
	{ GIMP_PDB_INT32,      "run_mode",         "Interactive, non-interactive" },
	{ GIMP_PDB_IMAGE,      "image",            "Input image" },
	{ GIMP_PDB_DRAWABLE,   "drawable",         "Input drawable" },
	{ GIMP_PDB_INT32,      "flags",            "1: Antialiasing 2: Supersampling 4: Animate 8: Periodic" },
	{ GIMP_PDB_INT32,      "frames",           "Number of frames" },
	{ GIMP_PDB_FLOAT,      "param_t",          "The parameter t (if not animating)" },
	{ GIMP_PDB_STRING,     "expression",       "MathMap expression" }
    };
    static GimpParamDef *return_vals  = NULL;
    static int nargs = sizeof(args) / sizeof(args[0]);
    static int nreturn_vals = 0;

    gimp_install_procedure("plug_in_mathmap",
			   "Generate an image using a mathematical expression.",
			   "Generates an image by means of a mathematical expression. The expression "
			   "can also refer to the data of an original image. Thus, arbitrary "
			   "distortions can be constructed. Even animations can be generated.",
			   "Mark Probst",
			   "Mark Probst",
			   MATHMAP_DATE ", " MATHMAP_VERSION,
			   "<Image>/Filters/Generic/MathMap/MathMap",
			   "RGB*, GRAY*",
			   GIMP_PLUGIN,
			   nargs,
			   nreturn_vals,
			   args,
			   return_vals);

    register_examples();
}

/*****/

#ifndef GIMP2
static void
run (char *name, int nparams, GimpParam *param, int *nreturn_vals, GimpParam **return_vals)
#else
static void
run (const gchar *name, gint nparams, const GimpParam *param, gint *nreturn_vals, GimpParam **return_vals)
#endif
{
    static GimpParam values[1];

    GimpPDBStatusType status;
    int pwidth, pheight;

    int mutable_expression = 1;

    fprintf(stderr, "started as %s\n", name);

    INIT_LOCALE("mathmap");

    if (strncmp(name, "mathmap_", 8) == 0)
    {
	char *exp = expression_for_symbol(name + 8, read_rc_file());

	fprintf(stderr, "found %s\n", exp);

	if (exp != 0)
	{
	    strcpy(mmvals.expression, exp);
	    mutable_expression = 0;
	}
    }

    status   = GIMP_PDB_SUCCESS;
    run_mode = param[0].data.d_int32;

    image_id = param[1].data.d_int32;
    layer_id = gimp_image_get_active_layer(image_id);

    values[0].type = GIMP_PDB_STATUS;
    values[0].data.d_status = status;

    *nreturn_vals = 1;
    *return_vals = values;

    /* Get the active drawable info */

    input_drawables[0].drawable = gimp_drawable_get(param[2].data.d_drawable);
    input_drawables[0].bpp = gimp_drawable_bpp(DRAWABLE_ID(input_drawables[0].drawable));
    input_drawables[0].row = input_drawables[0].col = -1;
    input_drawables[0].tile = 0;
    input_drawables[0].fast_image_source = 0;
    input_drawables[0].used = 1;

    output_bpp = input_drawables[0].bpp;

    tile_width = gimp_tile_width();
    tile_height = gimp_tile_height();

    img_width = gimp_drawable_width(DRAWABLE_ID(input_drawables[0].drawable));
    img_height = gimp_drawable_height(DRAWABLE_ID(input_drawables[0].drawable));

    gimp_drawable_mask_bounds(DRAWABLE_ID(input_drawables[0].drawable), &sel_x1, &sel_y1, &sel_x2, &sel_y2);

    sel_width = sel_x2 - sel_x1;
    sel_height = sel_y2 - sel_y1;

    /* Calculate preview size */

    if (sel_width > sel_height) {
	pwidth  = MIN(sel_width, PREVIEW_SIZE);
	pheight = sel_height * pwidth / sel_width;
    } else {
	pheight = MIN(sel_height, PREVIEW_SIZE);
	pwidth  = sel_width * pheight / sel_height;
    } /* else */

    preview_width  = MAX(pwidth, 2);  /* Min size is 2 */
    preview_height = MAX(pheight, 2);

    init_builtins();
    init_tags();
    init_macros();
    init_noise();
    init_compiler();

    /* See how we will run */

    switch (run_mode) {
	case GIMP_RUN_INTERACTIVE:
	    /* Possibly retrieve data */

	    gimp_get_data(name, &mmvals);

	    /* Get information from the dialog */

	    update_gradient();

	    if (!mathmap_dialog(mutable_expression))
		return;

	    break;

	case GIMP_RUN_NONINTERACTIVE:
	    /* Make sure all the arguments are present */

	    if (nparams != 7)
		status = GIMP_PDB_CALLING_ERROR;

	    if (status == GIMP_PDB_SUCCESS)
	    {
		mmvals.flags = param[3].data.d_int32;
		mmvals.frames = param[4].data.d_int32;
		mmvals.param_t = param[5].data.d_float;
		expression_copy(mmvals.expression, param[6].data.d_string);
	    }

	    break;

	case GIMP_RUN_WITH_LAST_VALS:
	    /* Possibly retrieve data */

	    gimp_get_data(name, &mmvals);
	    break;

	default:
	    break;
    } /* switch */

    /* Mathmap the image */

    if ((status == GIMP_PDB_SUCCESS)
	&& (gimp_drawable_is_rgb(DRAWABLE_ID(input_drawables[0].drawable))
	    || gimp_drawable_is_gray(DRAWABLE_ID(input_drawables[0].drawable))))
    {
	int animation_enabled = mmvals.flags & FLAG_ANIMATION;

	update_gradient();

	/* Set the tile cache size */
	gimp_tile_cache_ntiles((input_drawables[0].drawable->width + gimp_tile_width() - 1)
			       / gimp_tile_width());

	/* Run! */

	if (animation_enabled)
	{
	    int frame;

	    gimp_image_undo_group_start(image_id);
	    for (frame = 0; frame < mmvals.frames; ++frame)
	    {
		gint32 layer;
		char layer_name[100];
		float t;

		if (mmvals.flags & FLAG_PERIODIC)
		    t = (double)frame / (double)mmvals.frames;
		else if (mmvals.frames < 2)
		    t = 0.0;
		else
		    t = (double)frame / (double)(mmvals.frames - 1);
		layer = mathmap_layer_copy(layer_id);
		sprintf(layer_name, "Frame %d", frame + 1);
		gimp_drawable_set_name(layer, layer_name);
		output_drawable = gimp_drawable_get(layer);
		do_mathmap(frame, t);
		gimp_image_add_layer(image_id, layer, 0);
	    }
	    gimp_image_undo_group_end(image_id);
	}
	else
	{
	    output_drawable = input_drawables[0].drawable;
	    do_mathmap(-1, mmvals.param_t);
	}

	/* If run mode is interactive, flush displays */

	if (run_mode != GIMP_RUN_NONINTERACTIVE)
	    gimp_displays_flush();

	/* Store data */

	if (run_mode == GIMP_RUN_INTERACTIVE)
	    gimp_set_data(name, &mmvals, sizeof(mathmap_vals_t));
    } else if (status == GIMP_PDB_SUCCESS)
	status = GIMP_PDB_EXECUTION_ERROR;

    values[0].data.d_status = status;

    gimp_drawable_detach(input_drawables[0].drawable);

    printf("%ld pixels requested\n", num_pixels_requested);
} /* run */

/*****/

static gint32 
mathmap_layer_copy(gint32 layerID)
{
    GimpParam *return_vals;
    int nreturn_vals;
    gint32 nlayer;

    return_vals = gimp_run_procedure ("gimp_layer_copy", 
				      &nreturn_vals,
				      GIMP_PDB_LAYER, layerID,
				      GIMP_PDB_INT32, TRUE,
				      GIMP_PDB_END);
 
    if (return_vals[0].data.d_status == GIMP_PDB_SUCCESS)
	nlayer = return_vals[1].data.d_layer;
    else
	nlayer = -1;
    gimp_destroy_params(return_vals, nreturn_vals);
    return nlayer;
} 

/*****/

static void
update_gradient (void)
{
    gdouble *samples;
    int i;

#ifndef GIMP2
    samples = gimp_gradients_sample_uniform(USER_GRADIENT_POINTS);
#else
    samples = gimp_gradients_sample_uniform(USER_GRADIENT_POINTS, FALSE);
#endif

    for (i = 0; i < USER_GRADIENT_POINTS; ++i)
	gradient_samples[i] = MAKE_RGBA_COLOR_FLOAT(samples[i * 4 + 0], samples[i * 4 + 1],
						    samples[i * 4 + 2], samples[i * 4 + 3]);
}

/*****/

static int
generate_code (int current_frame, float current_t)
{
    if (expression_changed)
    {
	mathmap_t *new_mathmap;
	FILE *template;

	if (run_mode == GIMP_RUN_INTERACTIVE && expression_entry != 0)
	    dialog_text_update();

	if (mathmap != 0)
	    unload_mathmap(mathmap);

	template = open_rc_file("new_template.c");
	if (template == 0)
	{
	    sprintf(error_string, "Cannot read template file new_template.c");
	    new_mathmap = 0;
	}
	else
	{
	    new_mathmap = compile_mathmap(mmvals.expression, template);

	    fclose(template);
	}

	if (new_mathmap == 0)
	{
	    gimp_message(error_string);

	    /* FIXME: free old mathmap/invocation */

	    mathmap = 0;
	    invocation = 0;
	}
	else
	{
	    mathmap_invocation_t *new_invocation;

	    new_invocation = invoke_mathmap(new_mathmap, invocation, sel_width, sel_height);
	    assert(new_invocation != 0);

	    new_invocation->output_bpp = output_bpp;
	    new_invocation->origin_x = sel_x1;
	    new_invocation->origin_y = sel_y1;

	    if (invocation != 0)
	    {
		free_invocation(invocation);
		free_mathmap(mathmap);
	    }

	    mathmap = new_mathmap;
	    invocation = new_invocation;

	    expression_changed = 0;

	    update_userval_table();
	}
    }

    if (invocation != 0)
    {
	invocation->antialiasing = mmvals.flags & FLAG_ANTIALIASING;
	invocation->supersampling = mmvals.flags & FLAG_SUPERSAMPLING;

	invocation->current_frame = current_frame;
	invocation->current_t = current_t;

	invocation->edge_behaviour = edge_behaviour_mode;
#ifndef GIMP2
	invocation->edge_color = MAKE_RGBA_COLOR(edge_color[0], edge_color[1], edge_color[2], edge_color[3]);
#else
	invocation->edge_color = MAKE_RGBA_COLOR_FLOAT(edge_color.r, edge_color.g, edge_color.b, edge_color.a);
#endif

	update_image_internals(invocation);
    }

    return invocation != 0;
}

/*****/

static void
unref_tiles (void)
{
    int i;

    for (i = 0; i < MAX_INPUT_DRAWABLES; ++i)
	if (input_drawables[i].used != 0 && input_drawables[i].tile != 0)
	{
	    gimp_tile_unref(input_drawables[i].tile, FALSE);
	    input_drawables[i].tile = 0;
	}
}

/*****/

int
alloc_input_drawable (GimpDrawable *drawable)
{
    int i;

    for (i = 0; i < MAX_INPUT_DRAWABLES; ++i)
	if (!input_drawables[i].used)
	    break;
    if (i == MAX_INPUT_DRAWABLES)
	return -1;

    input_drawables[i].drawable = drawable;
    input_drawables[i].bpp = gimp_drawable_bpp(DRAWABLE_ID(drawable));
    input_drawables[i].row = -1;
    input_drawables[i].col = -1;
    input_drawables[i].tile = 0;
    input_drawables[i].fast_image_source = 0;
    input_drawables[i].used = 1;

    return i;
}

void
free_input_drawable (int index)
{
    assert(input_drawables[index].used);
    if (input_drawables[index].tile != 0)
    {
	gimp_tile_unref(input_drawables[index].tile, FALSE);
	input_drawables[index].tile = 0;
    }
    if (input_drawables[index].fast_image_source != 0)
    {
	g_free(input_drawables[index].fast_image_source);
	input_drawables[index].fast_image_source = 0;
    }
    input_drawables[index].drawable = 0;
    input_drawables[index].used = 0;
}

GimpDrawable*
get_input_drawable (int index)
{
    assert(input_drawables[index].used);

    return input_drawables[index].drawable;
}

/*****/

static void
do_mathmap (int frame_num, float current_t)
{
    GimpPixelRgn dest_rgn;
    gpointer pr;
    gint progress, max_progress;
    gchar progress_info[30];

    assert(invocation != 0);

    previewing = 0;

    if (generate_code(frame_num, current_t))
    {
	/* Initialize pixel region */

	gimp_pixel_rgn_init(&dest_rgn, output_drawable, sel_x1, sel_y1, sel_width, sel_height,
			    TRUE, TRUE);

	progress = 0;
	max_progress = sel_width * sel_height;

	if (frame_num >= 0)
	    sprintf(progress_info, _("Mathmapping frame %d..."), frame_num + 1);
	else
	    strcpy(progress_info, _("Mathmapping..."));
	gimp_progress_init(progress_info);

	for (pr = gimp_pixel_rgns_register(1, &dest_rgn);
	     pr != NULL; pr = gimp_pixel_rgns_process(pr))
	{
	    /*
	    if (invocation->supersampling)
	    {
		unsigned char *line1,
		    *line2,
		    *line3;

		dest_row = dest_rgn.data;

		line1 = (unsigned char*)malloc((sel_width + 1) * output_bpp);
		line2 = (unsigned char*)malloc(sel_width * output_bpp);
		line3 = (unsigned char*)malloc((sel_width + 1) * output_bpp);

		for (col = 0; col <= dest_rgn.w; ++col)
		{
		    invocation->current_x = col + dest_rgn.x - sel_x1 - invocation->middle_x;
		    invocation->current_y = -(0.0 + dest_rgn.y - sel_y1 - invocation->middle_y);
		    calc_ra(invocation);
		    update_pixel_internals(invocation);
		    write_tuple_to_pixel(call_invocation(invocation), line1 + col * output_bpp, output_bpp);
		}

		for (row = 0; row < dest_rgn.h; ++row)
		{
		    dest = dest_row;

		    for (col = 0; col < dest_rgn.w; ++col)
		    {
			invocation->current_x = col + dest_rgn.x - sel_x1 + 0.5 - invocation->middle_x;
			invocation->current_y = -(row + dest_rgn.y - sel_y1 + 0.5 - invocation->middle_y);
			calc_ra(invocation);
			update_pixel_internals(invocation);
			write_tuple_to_pixel(call_invocation(invocation), line2 + col * output_bpp, output_bpp);
		    }
		    for (col = 0; col <= dest_rgn.w; ++col)
		    {
			invocation->current_x = col + dest_rgn.x - sel_x1 - invocation->middle_x;
			invocation->current_y = -(row + dest_rgn.y - sel_y1 + 1.0 - invocation->middle_y);
			calc_ra(invocation);
			update_pixel_internals(invocation);
			write_tuple_to_pixel(call_invocation(invocation), line3 + col * output_bpp, output_bpp);
		    }
	    
		    for (col = 0; col < dest_rgn.w; ++col)
		    {
			for (i = 0; i < output_bpp; ++i)
			    dest[i] = (line1[col*output_bpp+i]
				       + line1[(col+1)*output_bpp+i]
				       + 2*line2[col*output_bpp+i]
				       + line3[col*output_bpp+i]
				       + line3[(col+1)*output_bpp+i]) / 6;
			dest += output_bpp;
		    }

		    memcpy(line1, line3, (sel_width + 1) * output_bpp);

		    dest_row += dest_rgn.rowstride;
		}
	    }
	    else
	    */
	    {
		invocation->img_width = dest_rgn.w;
		invocation->img_height = dest_rgn.h;
		invocation->row_stride = dest_rgn.rowstride;
		invocation->origin_x = dest_rgn.x - sel_x1;
		invocation->origin_y = dest_rgn.y - sel_y1;
		invocation->scale_x = invocation->scale_y = 1.0;
		invocation->output_bpp = gimp_drawable_bpp(DRAWABLE_ID(output_drawable));

		call_invocation(invocation, 0, dest_rgn.h, dest_rgn.data);

		/*
		dest_row = dest_rgn.data;

		for (row = dest_rgn.y; row < (dest_rgn.y + dest_rgn.h); row++)
		{
		    dest = dest_row;

		    for (col = dest_rgn.x; col < (dest_rgn.x + dest_rgn.w); col++)
		    {
			invocation->current_x = col - sel_x1 - invocation->middle_x;
			invocation->current_y = -(row - sel_y1 - invocation->middle_y);
			calc_ra(invocation);
			update_pixel_internals(invocation);
			write_tuple_to_pixel(call_invocation(invocation), dest, output_bpp);
			dest += output_bpp;
		    }
		
		    dest_row += dest_rgn.rowstride;
		}
		*/
	    }

	    /* Update progress */
	    progress += dest_rgn.w * dest_rgn.h;
	    gimp_progress_update((double) progress / max_progress);
	}

	unref_tiles();

	gimp_drawable_flush(output_drawable);
	gimp_drawable_merge_shadow(DRAWABLE_ID(output_drawable), TRUE);
	gimp_drawable_update(DRAWABLE_ID(output_drawable), sel_x1, sel_y1, sel_width, sel_height);
    }
} /* mathmap */

/*****/

color_t
mathmap_get_pixel (mathmap_invocation_t *invocation, int drawable_index, int frame, int x, int y)
{
    gint newcol, newrow;
    gint newcoloff, newrowoff;
    guchar *p;
    input_drawable_t *drawable;
    guchar r, g, b, a;

    ++num_pixels_requested;

    if (drawable_index < 0 || drawable_index >= MAX_INPUT_DRAWABLES)
	return invocation->edge_color;

    assert(input_drawables[drawable_index].used);

    if (x < 0 || x >= img_width
	|| y < 0 || y >= img_height)
	return invocation->edge_color;

    drawable = &input_drawables[drawable_index];
    assert(drawable->used);

    newcol = x / tile_width;
    newcoloff = x % tile_width;
    newrow = y / tile_height;
    newrowoff = y % tile_height;

    if (drawable->col != newcol || drawable->row != newrow || drawable->tile == NULL)
    {
	if (drawable->tile != NULL)
	    gimp_tile_unref(drawable->tile, FALSE);

	drawable->tile = gimp_drawable_get_tile(drawable->drawable, FALSE, newrow, newcol);
	assert(drawable->tile != 0);
	gimp_tile_ref(drawable->tile);

	drawable->col = newcol;
	drawable->row = newrow;
    }

    p = drawable->tile->data + drawable->tile->bpp * (drawable->tile->ewidth * newrowoff + newcoloff);

    if (drawable->bpp == 1 || drawable->bpp == 2)
	r = g = b = p[0];
    else if (drawable->bpp == 3 || drawable->bpp == 4)
    {
	r = p[0];
	g = p[1];
	b = p[2];
    }
    else
	assert(0);

    if (drawable->bpp == 1 || drawable->bpp == 3)
	a = 255;
    else
	a = p[drawable->bpp - 1];

    return MAKE_RGBA_COLOR(r, g, b, a);
}

color_t
mathmap_get_fast_pixel (mathmap_invocation_t *invocation, int drawable_index, int x, int y)
{
    input_drawable_t *drawable;

    if (drawable_index < 0 || drawable_index >= MAX_INPUT_DRAWABLES)
	return invocation->edge_color;

    assert(input_drawables[drawable_index].used);

    if (x < 0 || x >= preview_width
	|| y < 0 || y >= preview_height)
	return invocation->edge_color;

    drawable = &input_drawables[drawable_index];
    assert(drawable->used);

    if (drawable->fast_image_source == 0)
	build_fast_image_source(drawable);

    return drawable->fast_image_source[x + y * preview_width];
}

/*****/

static void
build_fast_image_source (input_drawable_t *drawable)
{
    color_t *p;
    int x, y;

    assert(drawable->fast_image_source == 0);

    p = drawable->fast_image_source = g_malloc(preview_width * preview_height * sizeof(color_t));

    for (y = 0; y < preview_height; ++y)
	for (x = 0; x < preview_width; ++x)
	    drawable->fast_image_source[x + y * preview_width] =
		mathmap_get_pixel(invocation,
				  drawable - input_drawables, 0,
				  sel_x1 + x * sel_width / preview_width,
				  sel_y1 + y * sel_height / preview_height);
}

/*****/

#ifndef GIMP2
static GtkWidget*
tree_from_lisp_object (GtkWidget *root_item, lisp_object_t *obj)
#else
static void
tree_from_lisp_object (GtkTreeStore *store,
		       GtkTreeIter *parent, lisp_object_t *obj)
#endif
{
#ifndef GIMP2
    GtkWidget *tree = gtk_tree_new();

    if (root_item != 0)
	gtk_tree_item_set_subtree(GTK_TREE_ITEM(root_item), tree);
#endif

    for (; lisp_type(obj) != LISP_TYPE_NIL; obj = lisp_cdr(obj))
    {
	lisp_object_t *vars[2];
#ifndef GIMP2
	GtkWidget *item = 0;
#else
	GtkTreeIter iter;
#endif

	if (lisp_match_string("(group #?(string) . #?(list))", lisp_car(obj), vars))
	{
#ifndef GIMP2
	    item = gtk_tree_item_new_with_label(lisp_string(vars[0]));
	    gtk_tree_append(GTK_TREE(tree), item);
	    gtk_widget_show(item);

	    tree_from_lisp_object(item, vars[1]);
#else
	    gtk_tree_store_append(store, &iter, parent);
	    gtk_tree_store_set(store, &iter, 0, lisp_string(vars[0]), -1);

	    tree_from_lisp_object(store, &iter, vars[1]);
#endif
	}
	else if (lisp_match_string("(expression #?(string) #?(string))", lisp_car(obj), vars))
	{
#ifndef GIMP2
	    item = gtk_tree_item_new_with_label(lisp_string(vars[0]));
	    gtk_tree_append(GTK_TREE(tree), item);
	    gtk_widget_show(item);
	    gtk_object_set_user_data(GTK_OBJECT(item),
				     strcpy((char*)malloc(strlen(lisp_string(vars[1])) + 1),
					    lisp_string(vars[1])));
#else
	    gtk_tree_store_append(store, &iter, parent);
	    gtk_tree_store_set(store, &iter, 0, lisp_string(vars[0]),
			       1, lisp_string(vars[1]), -1);
#endif
	}
	else
	{
	    fprintf(stderr, "illegal expression: ");
	    lisp_dump(obj, stderr);
	    assert(0);
	}
    }

#ifndef GIMP2
    gtk_widget_show(tree);

    if (root_item != 0)
	gtk_tree_item_expand(GTK_TREE_ITEM(root_item));

    return tree;
#endif
}

#ifndef GIMP2
static GtkWidget*
read_tree_from_rc (void)
{
    GtkWidget *tree;
    lisp_object_t *obj = read_rc_file();

    if (obj == 0)
    {
	tree = gtk_tree_new();
	gtk_widget_show(tree);
	return tree;
    }

    tree = tree_from_lisp_object(0, obj);
    lisp_free(obj);

    return tree;
}
#else
static GtkWidget*
read_tree_from_rc (void)
{
    GtkTreeStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    GtkWidget *tree;
    lisp_object_t *obj;

    /* model */
    store = gtk_tree_store_new(2, G_TYPE_STRING, G_TYPE_STRING);

    obj = read_rc_file();
    if(obj)
    {
    	tree_from_lisp_object(store, NULL, obj);
    	lisp_free(obj);
    }

    /* view */
    tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);
    g_signal_connect(G_OBJECT(selection), "changed",
		     G_CALLBACK(dialog_tree_changed),
		     (gpointer)NULL);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("Examples", renderer,
		    				      "text", 0,
						      NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

    gtk_widget_show(tree);

    return tree;
}
#endif

/*****/

static void
update_userval_table (void)
{
    if (uservalues_table != 0)
    {
	gtk_container_remove(GTK_CONTAINER(GTK_BIN(uservalues_scrolled_window)->child), uservalues_table);
	uservalues_table = 0;
    }

    uservalues_table = make_userval_table(mathmap->userval_infos, invocation->uservals);

    if (uservalues_table != 0)
    {
#if GTK_MAJOR_VERSION < 1 || (GTK_MAJOR_VERSION == 1 && GTK_MINOR_VERSION < 1)
	gtk_container_add(GTK_CONTAINER(uservalues_scrolled_window), uservalues_table);
#else
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(uservalues_scrolled_window), uservalues_table);
#endif
    }
}


/*****/

#define RESPONSE_ABOUT 1

static gint
mathmap_dialog (int mutable_expression)
{
    static int edge_behaviour_color = EDGE_BEHAVIOUR_COLOR;
    static int edge_behaviour_wrap = EDGE_BEHAVIOUR_WRAP;
    static int edge_behaviour_reflect = EDGE_BEHAVIOUR_REFLECT;

    GtkWidget *dialog;
    GtkWidget *top_table, *middle_table;
    GtkWidget *vbox;
    GtkWidget *frame;
    GtkWidget *table;
    GtkWidget *button;
    GtkWidget *label;
    GtkWidget *toggle;
    GtkWidget *alignment;
    GtkWidget *root_tree;
    GtkWidget *scale;
    GtkWidget *vscrollbar;
    GtkWidget *notebook;
    GtkObject *adjustment;
    GSList *edge_group = 0;
#ifndef GIMP2
    guchar *color_cube;
    int position = 0;
#else
    guchar color_cube[4] = { 6, 6, 4, 24 };
#endif

    gimp_ui_init("mathmap", TRUE);

    gtk_preview_set_gamma(gimp_gamma());
    gtk_preview_set_install_cmap(gimp_install_cmap());
#ifndef GIMP2
    color_cube = gimp_color_cube();
#endif
    gtk_preview_set_color_cube(color_cube[0], color_cube[1], color_cube[2], color_cube[3]);

    gtk_widget_set_default_visual(gtk_preview_get_visual());
    gtk_widget_set_default_colormap(gtk_preview_get_cmap());

    wint.wimage = g_malloc(preview_width * preview_height * 3 * sizeof(guchar));

#ifndef GIMP2
    dialog = gimp_dialog_new ("MathMap", "mathmap",
			      NULL, NULL,
			      GTK_WIN_POS_MOUSE,
			      FALSE, TRUE, FALSE,

			      _("OK"), dialog_ok_callback,
			      NULL, NULL, NULL, TRUE, FALSE,
			      _("Cancel"), gtk_widget_destroy,
			      NULL, 1, NULL, FALSE, TRUE,
			      _("About"), dialog_about_callback,
			      NULL, NULL, NULL, FALSE, FALSE,
			      _("Help"), dialog_help_callback,
			      NULL, NULL, NULL, FALSE, FALSE,

			      NULL);

    gtk_signal_connect(GTK_OBJECT(dialog), "destroy",
		       (GtkSignalFunc) gtk_main_quit,
		       NULL);
#else
    dialog = gimp_dialog_new("MathMap", "mathmap",
			     NULL, 0,
			     gimp_standard_help_func, "plug-in-mathmap",
			     GTK_STOCK_HELP, GTK_RESPONSE_HELP,
			     _("About"), RESPONSE_ABOUT,
			     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			     GTK_STOCK_OK, GTK_RESPONSE_OK,
			     NULL);

    g_signal_connect (dialog, "response",
		      G_CALLBACK (dialog_response),
		      NULL);

    g_signal_connect (dialog, "destroy",
		      G_CALLBACK (gtk_main_quit),
		      NULL);
#endif

    top_table = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), top_table, TRUE, TRUE, 0);
    gtk_widget_show(top_table);

    /* Preview */

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(top_table), vbox, FALSE, FALSE, 0);
    gtk_widget_show(vbox);
    frame = gtk_frame_new(NULL);
    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
    gtk_box_pack_start(GTK_BOX(vbox), frame, FALSE, FALSE, 0);
    gtk_widget_show(frame);

    wint.preview = gtk_preview_new(GTK_PREVIEW_COLOR);
    gtk_preview_size(GTK_PREVIEW(wint.preview), preview_width, preview_height);
    gtk_container_add(GTK_CONTAINER(frame), wint.preview);
    gtk_widget_show(wint.preview);

    button = gtk_button_new_with_label(_("Preview"));
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       (GtkSignalFunc)dialog_preview_callback, 0);
    gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
    gtk_widget_show(button);

    /* Notebook */

    notebook = gtk_notebook_new();
    gtk_notebook_set_tab_pos (GTK_NOTEBOOK (notebook), GTK_POS_TOP);
    gtk_box_pack_start(GTK_BOX(top_table), notebook, TRUE, TRUE, 0);
    gtk_widget_show(notebook);

	/* Settings */

	middle_table = gtk_table_new(3, 2, FALSE);
	gtk_container_border_width(GTK_CONTAINER(middle_table), 6);
	gtk_table_set_col_spacings(GTK_TABLE(middle_table), 4);
	gtk_widget_show(middle_table);

            /* Sampling */

            table = gtk_table_new(2, 1, FALSE);
	    gtk_container_border_width(GTK_CONTAINER(table), 6);
	    gtk_table_set_row_spacings(GTK_TABLE(table), 4);
    
	    frame = gtk_frame_new(NULL);
	    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
	    gtk_container_add(GTK_CONTAINER(frame), table);
	    gtk_table_attach(GTK_TABLE(middle_table), frame, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);

	    gtk_widget_show(table);
	    gtk_widget_show(frame);

                /* Antialiasing */

		toggle = gtk_check_button_new_with_label(_("Antialiasing"));
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle),
					    mmvals.flags & FLAG_ANTIALIASING);
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_antialiasing_update, 0);
		gtk_widget_show(toggle);

		/* Supersampling */
	    
		toggle = gtk_check_button_new_with_label(_("Supersampling"));
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle),
					    mmvals.flags & FLAG_SUPERSAMPLING);
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_supersampling_update, 0);
		gtk_widget_show(toggle);

	    /* Preview Options */

            table = gtk_table_new(2, 1, FALSE);
	    gtk_container_border_width(GTK_CONTAINER(table), 6);
	    gtk_table_set_row_spacings(GTK_TABLE(table), 4);
    
	    frame = gtk_frame_new(NULL);
	    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
	    gtk_container_add(GTK_CONTAINER(frame), table);
	    gtk_table_attach(GTK_TABLE(middle_table), frame, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);

	    gtk_widget_show(table);
	    gtk_widget_show(frame);

	        /* Auto Preview */

	        toggle = gtk_check_button_new_with_label(_("Auto Preview"));
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle), auto_preview);
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_auto_preview_update, 0);
		gtk_widget_show(toggle);

	        /* Fast Preview */

		toggle = gtk_check_button_new_with_label(_("Fast Preview"));
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle), fast_preview);
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_fast_preview_update, 0);
		gtk_widget_show(toggle);

	    /* Edge Behaviour */

	    table = gtk_table_new(2, 3, FALSE);
	    gtk_container_border_width(GTK_CONTAINER(table), 6);
	    gtk_table_set_row_spacings(GTK_TABLE(table), 4);

	    frame = gtk_frame_new(_("Edge Behaviour"));
	    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
	    gtk_container_add(GTK_CONTAINER(frame), table);
	    gtk_table_attach(GTK_TABLE(middle_table), frame, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);

	    gtk_widget_show(table);
	    gtk_widget_show(frame);

	        /* Color */

	        toggle = gtk_radio_button_new_with_label(edge_group, _("Color"));
		edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_edge_behaviour_update, &edge_behaviour_color);
		gtk_widget_show(toggle);

#ifndef GIMP2
		edge_color_well = gimp_color_button_new(_("Edge Color"), 32, 16,
							edge_color, 4);
#else
		edge_color_well = gimp_color_button_new(_("Edge Color"), 32, 16,
							&edge_color, GIMP_COLOR_AREA_SMALL_CHECKS);
#endif
		gtk_signal_connect(GTK_OBJECT(edge_color_well), "color_changed",
				   (GtkSignalFunc)dialog_edge_color_changed, 0);
		gtk_widget_show(edge_color_well);
		gtk_table_attach(GTK_TABLE(table), edge_color_well, 1, 2, 0, 1, GTK_FILL, 0, 0, 0);

	        /* Wrap */

	        toggle = gtk_radio_button_new_with_label(edge_group, _("Wrap"));
		edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_edge_behaviour_update, &edge_behaviour_wrap);
		gtk_widget_show(toggle);

	        /* Reflect */

	        toggle = gtk_radio_button_new_with_label(edge_group, _("Reflect"));
		edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_edge_behaviour_update, &edge_behaviour_reflect);
		gtk_widget_show(toggle);

	    /* Animation */
	    
	    table = gtk_table_new(4, 1, FALSE);
	    gtk_container_border_width(GTK_CONTAINER(table), 6);
	    gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	    gtk_table_set_col_spacings(GTK_TABLE(table), 4);
    
	    frame = gtk_frame_new(NULL);
	    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
	    gtk_container_add(GTK_CONTAINER(frame), table);

	    alignment = gtk_alignment_new(0, 0, 0, 0);
	    gtk_container_add(GTK_CONTAINER(alignment), frame);
	    gtk_table_attach(GTK_TABLE(middle_table), alignment, 1, 2, 0, 3, GTK_FILL, 0, 0, 0);

	    gtk_widget_show(table);
	    gtk_widget_show(frame);
	    gtk_widget_show(alignment);

	        /* Animation Toggle */

	        alignment = gtk_alignment_new(0, 0, 0, 0);
		toggle = gtk_check_button_new_with_label(_("Animate"));
		gtk_container_add(GTK_CONTAINER(alignment), toggle);
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle),
					    mmvals.flags & FLAG_ANIMATION);
		gtk_table_attach(GTK_TABLE(table), alignment, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_animation_update, 0);
		gtk_widget_show(toggle);
		gtk_widget_show(alignment);

		/* Number of Frames */

		frame_table = gtk_table_new(1, 2, FALSE);
		gtk_table_set_col_spacings(GTK_TABLE(frame_table), 4);
		gtk_table_attach(GTK_TABLE(table), frame_table, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);

		label = gtk_label_new(_("Frames"));
		gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
		gtk_table_attach(GTK_TABLE(frame_table), label, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		adjustment = gtk_adjustment_new(mmvals.frames, 2, 100, 1.0, 1.0, 0.0);
		scale = gtk_hscale_new(GTK_ADJUSTMENT(adjustment));
		gtk_widget_set_usize(scale, 100, 0);
		gtk_table_attach (GTK_TABLE (frame_table), scale, 1, 2, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_scale_set_value_pos (GTK_SCALE (scale), GTK_POS_TOP);
		gtk_scale_set_digits(GTK_SCALE(scale),0);
		gtk_range_set_update_policy (GTK_RANGE (scale), GTK_UPDATE_DELAYED);
		gtk_signal_connect (GTK_OBJECT (adjustment), "value_changed",
				    (GtkSignalFunc) dialog_scale_update,
				    &mmvals.frames);
		gtk_widget_show(label);
		gtk_widget_show(scale);

		gtk_widget_show(frame_table);
		gtk_widget_set_sensitive(frame_table, mmvals.flags & FLAG_ANIMATION);

		/* Periodic */

	        alignment = gtk_alignment_new(0, 0, 0, 0);
		toggle = gtk_check_button_new_with_label(_("Periodic"));
		gtk_container_add(GTK_CONTAINER(alignment), toggle);
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle), mmvals.flags & FLAG_PERIODIC);
		gtk_table_attach(GTK_TABLE(table), alignment, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_periodic_update, 0);
		gtk_widget_show(toggle);
		gtk_widget_show(alignment);

		/* t */

		t_table = gtk_table_new(1, 2, FALSE);
		gtk_table_set_col_spacings(GTK_TABLE(t_table), 4);
		gtk_table_attach(GTK_TABLE(table), t_table, 0, 1, 4, 5, GTK_FILL, 0, 0, 0);

		label = gtk_label_new(_("Parameter t"));
		gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
		gtk_table_attach(GTK_TABLE(t_table), label, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		adjustment = gtk_adjustment_new(mmvals.param_t, 0.0, 1.0, 0.01, 0.1, 0.0);
		scale = gtk_hscale_new(GTK_ADJUSTMENT(adjustment));
		gtk_widget_set_usize(scale, 100, 0);
		gtk_table_attach (GTK_TABLE (t_table), scale, 1, 2, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_scale_set_value_pos (GTK_SCALE (scale), GTK_POS_TOP);
		gtk_scale_set_digits(GTK_SCALE(scale),2);
		gtk_range_set_update_policy (GTK_RANGE (scale), GTK_UPDATE_CONTINUOUS);
		gtk_signal_connect (GTK_OBJECT (adjustment), "value_changed",
				    (GtkSignalFunc) dialog_t_update,
				    &mmvals.param_t);
		gtk_widget_show(label);
		gtk_widget_show(scale);

		gtk_widget_show(t_table);
		gtk_widget_set_sensitive(t_table, !(mmvals.flags & FLAG_ANIMATION));

	label = gtk_label_new(_("Settings"));
	gtk_widget_show(label);
	gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), middle_table, label, label);

        /* Expression */

	if (mutable_expression)
	{
#ifndef GIMP2
	    table = gtk_hbox_new(FALSE, 0);
	    gtk_widget_show(table);

	    label = gtk_label_new(_("Expression"));
	    gtk_widget_show(label);
	    gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), table, label, label);

	    expression_entry = gtk_text_new(NULL, NULL);
	    gtk_signal_connect(GTK_OBJECT(expression_entry), "changed",
			       (GtkSignalFunc)dialog_text_changed,
			       (gpointer)NULL);
	    gtk_text_set_editable(GTK_TEXT(expression_entry), TRUE);
#else
	    GtkTextBuffer *buffer;
	    PangoFontDescription *font_desc;

	    table = gtk_scrolled_window_new (NULL, NULL);
	    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(table),
					    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	    gtk_widget_show (table);

	    expression_entry = gtk_text_view_new();
	    gtk_container_add(GTK_CONTAINER(table), expression_entry);
	    gtk_text_view_set_editable(GTK_TEXT_VIEW(expression_entry), TRUE);
	    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(expression_entry),
			    		GTK_WRAP_CHAR);
#endif

#ifndef GIMP2
	    gtk_box_pack_start(GTK_BOX(table), expression_entry, TRUE, TRUE, 0);
	    gtk_widget_show(expression_entry);
	    gtk_widget_realize(expression_entry);
	    gtk_editable_insert_text(GTK_EDITABLE(expression_entry), mmvals.expression,
				     strlen(mmvals.expression), &position);

	    vscrollbar = gtk_vscrollbar_new(GTK_TEXT(expression_entry)->vadj);
	    gtk_box_pack_start(GTK_BOX(table), vscrollbar, FALSE, FALSE, 0);
	    gtk_widget_show (vscrollbar);
#else
	    gtk_widget_show(expression_entry);

	    label = gtk_label_new(_("Expression"));
	    gtk_widget_show(label);

	    gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook),
			    		  table, label, label);

	    font_desc = pango_font_description_from_string("Courier 10");
	    gtk_widget_modify_font(expression_entry, font_desc);
	    pango_font_description_free(font_desc);

	    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(expression_entry));
	    g_signal_connect(G_OBJECT(buffer), "changed",
			     G_CALLBACK(dialog_text_changed),
			     (gpointer)NULL);
	    gtk_text_buffer_set_text(buffer, mmvals.expression,
			    	     strlen(mmvals.expression));

	    vscrollbar = gtk_vscrollbar_new(GTK_TEXT_VIEW(expression_entry)->vadjustment);
	    gtk_widget_realize(expression_entry);
#endif
	}

	/* User Values */

	uservalues_scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(uservalues_scrolled_window),
					GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_show(uservalues_scrolled_window);

	uservalues_table = 0;

	label = gtk_label_new(_("User Values"));
	gtk_widget_show(label);
	gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), uservalues_scrolled_window, label, label);

	/* Examples */

	if (mutable_expression)
	{
	    table = gtk_scrolled_window_new (NULL, NULL);
	    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(table),
					    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	    gtk_widget_show (table);

	    root_tree = read_tree_from_rc();
#ifndef GIMP2
	    gtk_signal_connect(GTK_OBJECT(root_tree), "selection_changed",
			       (GtkSignalFunc)dialog_tree_changed,
			       (gpointer)NULL);
#endif
#if GTK_MAJOR_VERSION < 1 || (GTK_MAJOR_VERSION == 1 && GTK_MINOR_VERSION < 1)
	    gtk_container_add(GTK_CONTAINER(table), root_tree);
#else
	    gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(table), root_tree);
#endif
#ifndef GIMP2
	    gtk_tree_set_selection_mode(GTK_TREE(root_tree), GTK_SELECTION_BROWSE);
	    gtk_tree_set_view_lines(GTK_TREE(root_tree), FALSE);
	    gtk_tree_set_view_mode(GTK_TREE(root_tree), FALSE);
#else
	    gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(root_tree)), GTK_SELECTION_BROWSE);
#endif
	    gtk_widget_show(root_tree);

	    label = gtk_label_new(_("Examples"));
	    gtk_widget_show(label);
	    gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), table, label, label);
	}

    /* Done */

    if (!mutable_expression)
	dialog_update_preview();

    gtk_widget_show(dialog);

#ifndef GIMP2
    gtk_main();
    gdk_flush();
#else
    gtk_main();
    gdk_flush();
    /* gimp_dialog_run(GIMP_DIALOG(dialog)); */
#endif

    unref_tiles();

    g_free(wint.wimage);

    return wint.run;
} /* mathmap_dialog */


/*****/

void
user_value_changed (void)
{
    if (auto_preview)
	dialog_update_preview();
}

static void
dialog_update_preview (void)
{
    if (generate_code(0, mmvals.param_t))
    {
	int x, y;
	guchar *p_ul, *p;
	gint check, check_0, check_1; 
	guchar *buf = (guchar*)malloc(4 * preview_width * preview_height);

	assert(buf != 0);

	update_uservals(mathmap->userval_infos, invocation->uservals);

	previewing = fast_preview;

	invocation->img_width = preview_width;
	invocation->img_height = preview_height;
	invocation->row_stride = preview_width * 4;
	invocation->origin_x = invocation->origin_y = 0;
	invocation->scale_x = (float)sel_width / (float)preview_width;
	invocation->scale_y = (float)sel_height / (float)preview_height;
	invocation->output_bpp = 4;

	call_invocation(invocation, 0, preview_height, buf);

	p = buf;
	p_ul = wint.wimage;

	for (y = 0; y < preview_height; y++)
	{
	    if ((y / CHECK_SIZE) & 1) {
		check_0 = CHECK_DARK;
		check_1 = CHECK_LIGHT;
	    } else {
		check_0 = CHECK_LIGHT;
		check_1 = CHECK_DARK;
	    }                        

	    for (x = 0; x < preview_width; x++)
	    {
		if (output_bpp == 2 || output_bpp == 4 )
		{
		    if (((x) / CHECK_SIZE) & 1)
			check = check_0;
		    else
			check = check_1;

		    if (p[3] == 255)
		    {
			p_ul[0] = p[0];
			p_ul[1] = p[1];
			p_ul[2] = p[2];
		    }
		    else if (p[3] != 255)
		    {
			float alphaf = (float)p[3] / 255.0;

			p_ul[0] = check + (p[0] - check) * alphaf;
			p_ul[1] = check + (p[1] - check) * alphaf;
			p_ul[2] = check + (p[2] - check) * alphaf;
		    }
		}
		else
		{
		    p_ul[0] = p[0];
		    p_ul[1] = p[1];
		    p_ul[2] = p[2];
		}

		p_ul += 3;
		p += 4;
	    }
	}

	free(buf);

	p = wint.wimage;

	for (y = 0; y < preview_height; y++)
	{
	    gtk_preview_draw_row(GTK_PREVIEW(wint.preview), p, 0, y, preview_width);

	    p += preview_width * 3;
	}

	gtk_widget_draw(wint.preview, NULL);
	gdk_flush();
    }
} /* dialog_update_preview */


/*****/

static void
dialog_scale_update (GtkAdjustment *adjustment, gint *value)
{
    *value = (gint)adjustment->value;
} /* dialog_scale_update */


/*****/

static void
dialog_t_update (GtkAdjustment *adjustment, gfloat *value)
{
    *value = (gfloat)adjustment->value;

    if (auto_preview)
	dialog_update_preview();
} /* dialog_scale_update */


/*****/

#ifndef GIMP2
static void
dialog_text_changed (void)
#else
static void
dialog_text_changed (GtkTextBuffer *buffer, gpointer user_data)
#endif
{
    expression_changed = 1;
}

/*****/

#ifndef GIMP2
static void
dialog_text_update (void)
{
    guint length = gtk_text_get_length(GTK_TEXT(expression_entry));
    char *expression = (char*)malloc(length + 1);
    int i;

    for (i = 0; i < length; ++i)
	expression[i] = GTK_TEXT_INDEX(GTK_TEXT(expression_entry), i);
    expression[i] = '\0';

    expression_copy(mmvals.expression, expression);

    free(expression);
} /* dialog_text_update */
#else
static void
dialog_text_update (void)
{
    GtkTextBuffer * buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(expression_entry));
    GtkTextIter start, end;
    gchar * expression;

    gtk_text_buffer_get_bounds(buffer, &start, &end);
    expression = gtk_text_buffer_get_text(buffer, &start, &end, TRUE);

    expression_copy(mmvals.expression, expression);

    g_free(expression);
} /* dialog_text_update */
#endif

/*****/

static void
dialog_supersampling_update (GtkWidget *widget, gpointer data)
{
    mmvals.flags &= ~FLAG_SUPERSAMPLING;

    if (GTK_TOGGLE_BUTTON(widget)->active)
	mmvals.flags |= FLAG_SUPERSAMPLING;
}

/*****/

static void
dialog_auto_preview_update (GtkWidget *widget, gpointer data)
{
    auto_preview = GTK_TOGGLE_BUTTON(widget)->active;
}

/*****/

static void
dialog_fast_preview_update (GtkWidget *widget, gpointer data)
{
    fast_preview = GTK_TOGGLE_BUTTON(widget)->active;
    if (auto_preview)
	dialog_update_preview();
}

/*****/

static void
dialog_edge_behaviour_update (GtkWidget *widget, gpointer data)
{
    edge_behaviour_mode = *(int*)data;
    if (edge_behaviour_mode == EDGE_BEHAVIOUR_COLOR)
	gtk_widget_set_sensitive(edge_color_well, 1);
    else
	gtk_widget_set_sensitive(edge_color_well, 0);

    if (auto_preview)
	dialog_update_preview();
}

static void
dialog_edge_color_changed (GtkWidget *color_well, gpointer data)
{
#ifdef GIMP2
    gimp_color_button_get_color(GIMP_COLOR_BUTTON(color_well), &edge_color);
#endif
    if (auto_preview)
	dialog_update_preview();
}

/*****/

static void
dialog_antialiasing_update (GtkWidget *widget, gpointer data)
{
    mmvals.flags &= ~FLAG_ANTIALIASING;

    if (GTK_TOGGLE_BUTTON(widget)->active)
	mmvals.flags |= FLAG_ANTIALIASING;

    if (auto_preview)
	dialog_update_preview();
}

/*****/

static void
dialog_animation_update (GtkWidget *widget, gpointer data)
{
    mmvals.flags &= ~FLAG_ANIMATION;

    if (GTK_TOGGLE_BUTTON(widget)->active)
	mmvals.flags |= FLAG_ANIMATION;

    gtk_widget_set_sensitive(frame_table, mmvals.flags & FLAG_ANIMATION);
    gtk_widget_set_sensitive(t_table, !(mmvals.flags & FLAG_ANIMATION));
}

/*****/

static void
dialog_periodic_update (GtkWidget *widget, gpointer data)
{
    mmvals.flags &= ~FLAG_PERIODIC;

    if (GTK_TOGGLE_BUTTON(widget)->active)
	mmvals.flags |= FLAG_PERIODIC;
}

/*****/

static void
dialog_preview_callback (GtkWidget *widget, gpointer data)
{
    update_gradient();
    dialog_update_preview();
}

/*****/

static void
dialog_ok_callback (GtkWidget *widget, gpointer data)
{
    if (generate_code(0, 0))
    {
	wint.run = TRUE;
	gtk_widget_destroy(GTK_WIDGET(data));
    }
} /* dialog_ok_callback */

/*****/

static void
dialog_help_callback (GtkWidget *widget, gpointer data)
{
    char *proc_blurb, *proc_help, *proc_author, *proc_copyright, *proc_date;
    int nparams, nreturn_vals;
    GimpParamDef *params, *return_vals;
    gint baz;
    GimpPDBProcType proc_type;

    if (gimp_procedural_db_proc_info("extension_web_browser",
				     &proc_blurb, &proc_help, 
				     &proc_author, &proc_copyright, &proc_date,
				     &proc_type, &nparams, &nreturn_vals,
				     &params, &return_vals))
	gimp_run_procedure("extension_web_browser", &baz,
			   GIMP_PDB_INT32, GIMP_RUN_NONINTERACTIVE,
			   GIMP_PDB_STRING, MATHMAP_MANUAL_URL,
			   GIMP_PDB_INT32, 1,
			   GIMP_PDB_END);
    else 
    {
	gchar *message = g_strdup_printf(_("See %s"), MATHMAP_MANUAL_URL);

	gimp_message(message);
	g_free(message);
    }                                            
} /* dialog_help_callback */

/*****/

static void
dialog_about_callback (GtkWidget *widget, gpointer data)
{
    gchar *message = g_strdup_printf("Mathmap %s\n%s",
				     MATHMAP_VERSION,
				     _("written by\n"
				       "Mark Probst <schani@complang.tuwien.ac.at>"));

    gimp_message(message);
    g_free(message);
} /* dialog_about_callback */

/*****/

#ifdef GIMP2
static void
dialog_response (GtkWidget *widget,
                 gint response_id,
                 gpointer data)
{
    switch (response_id)
    {
	case RESPONSE_ABOUT :
	    dialog_about_callback(0, 0);
	    break;

	case GTK_RESPONSE_OK :
	    dialog_ok_callback(0, widget);
	    break;

	case GTK_RESPONSE_CANCEL :
	    gtk_widget_destroy(widget);
	    break;

	case GTK_RESPONSE_HELP :
	    dialog_help_callback(0, 0);
	    break;

	default :
	    assert(0);
    }
}
#endif

/****/

#ifndef GIMP2
static void
dialog_tree_changed (GtkTree *tree)
{
    GList *selection;
    GtkWidget *tree_item;

    selection = GTK_TREE_SELECTION(tree);

    if (g_list_length(selection) != 1)
	return;

    tree_item = GTK_WIDGET(selection->data);

    if (gtk_object_get_user_data(GTK_OBJECT(tree_item)) != 0)
    {
	char *expression = (char*)gtk_object_get_user_data(GTK_OBJECT(tree_item));
	gint position = 0;

	tree_item = GTK_WIDGET(selection->data);
	
	gtk_editable_delete_text(GTK_EDITABLE(expression_entry), 0,
				 gtk_text_get_length(GTK_TEXT(expression_entry)));
	gtk_editable_insert_text(GTK_EDITABLE(expression_entry), expression, strlen(expression),
				 &position);

	expression_copy(mmvals.expression, expression);
    }

    if (auto_preview)
	dialog_update_preview();
}
#else
static void
dialog_tree_changed (GtkTreeSelection *selection, gpointer data)
{
    GtkTreeModel * model;
    GtkTreeIter iter;

    if (selection == 0)
	return;

    if (gtk_tree_selection_get_selected(selection, &model, &iter))
    {
	GtkTextBuffer *buffer;
	GValue value = { 0, };
	const gchar *expression;

	gtk_tree_model_get_value(model, &iter, 1, &value);
	expression = g_value_get_string(&value);

	if (expression == 0)
	    return;

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(expression_entry));
	gtk_text_buffer_set_text(buffer, expression, strlen(expression));

	expression_copy(mmvals.expression, expression);
    }

    if (auto_preview)
	dialog_update_preview();
}
#endif
