/* The GIMP -- an image manipulation program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * MathMap plug-in --- generate an image by means of a mathematical expression
 * Copyright (C) 1997-2008 Mark Probst
 * schani@complang.tuwien.ac.at
 *
 * Plug-In structure based on:
 *   Whirl plug-in --- distort an image into a whirlpool
 *   Copyright (C) 1997 Federico Mena Quintero
 *   federico@nuclecu.unam.mx
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
#include <sys/stat.h>
#include <sys/types.h>
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
#include <gsl/gsl_errno.h>
#include <gtksourceview/gtksourceview.h>
#include <gtksourceview/gtksourcelanguage.h>
#include <gtksourceview/gtksourcelanguagesmanager.h>

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
#include "expression_db.h"
#include "designer/designer.h"

#define INIT_LOCALE(x)
#define _(x)             (x)

#define DEFAULT_PREVIEW_SIZE	384

/* Even more stuff from Quartics plugins */
#define CHECK_SIZE  8
#define CHECK_DARK  ((int) (1.0 / 3.0 * 255))
#define CHECK_LIGHT ((int) (2.0 / 3.0 * 255))   

#define EXPRESSIONS_DIR         "expressions"

#define DEFAULT_EXPRESSION      "filter ident (image in)\n  in(xy)\nend"
#define DEFAULT_NUMBER_FRAMES   10

#define FLAG_ANTIALIASING       1
#define FLAG_SUPERSAMPLING      2
#define FLAG_ANIMATION          4
#define FLAG_PERIODIC           8

#define MAX_EXPRESSION_LENGTH   65536

/***** Types *****/

typedef struct {
    gint flags;
    gint frames;
    gfloat param_t;
    gchar expression[MAX_EXPRESSION_LENGTH];
} mathmap_vals_t;

typedef struct {
    GtkWidget *preview;
    GdkPixbuf *pixbuf;
    guchar *wimage;
    gint run;
} mathmap_interface_t;

/***** Prototypes *****/

static void query (void);
static void run (const gchar *name,
		 gint nparams,
		 const GimpParam *param,
		 gint *nreturn_vals,
 		 GimpParam **return_vals);

static void expression_copy (gchar *dest, const gchar *src);

static int generate_code (int current_frame, float current_t);

static void do_mathmap (int frame_num, float t);
static gint32 mathmap_layer_copy (gint32 layerID);

extern int yyparse (void);

static void update_userval_table (void);

static void update_gradient (void);
static gint mathmap_dialog (int);
static void dialog_update_preview (void);
static void dialog_scale_update (GtkAdjustment *adjustment, gint *value);
static void dialog_t_update (GtkAdjustment *adjustment, gfloat *value);
static void dialog_text_changed (GtkTextBuffer * buffer, gpointer user_data);
static void dialog_text_update (void);
static void dialog_antialiasing_update (GtkWidget *widget, gpointer data);
static void dialog_supersampling_update (GtkWidget *widget, gpointer data);
static void dialog_auto_preview_update (GtkWidget *widget, gpointer data);
static void dialog_fast_preview_update (GtkWidget *widget, gpointer data);
static void dialog_edge_behaviour_update (GtkWidget *widget, gpointer data);
static void dialog_edge_color_changed (GtkWidget *color_well, gpointer data);
static void dialog_animation_update (GtkWidget *widget, gpointer data);
static void dialog_periodic_update (GtkWidget *widget, gpointer data);

static void calc_preview_size (int max_width, int max_height, int *width, int *height);
static gboolean alloc_preview_pixbuf (int width, int height);
static void dialog_preview_size_allocate (GtkWidget *widget, GtkAllocation *allocation, gpointer user_data);
static void dialog_preview_callback (GtkWidget *widget, gpointer data);
/*static void dialog_preview_click (GtkWidget *widget, GdkEvent *event);*/
static void refresh_preview (void);

static void dialog_save_callback (GtkWidget *widget, gpointer data);
static void dialog_save_as_callback (GtkWidget *widget, gpointer data);
static void dialog_ok_callback (GtkWidget *widget, gpointer data);
static void dialog_help_callback (GtkWidget *widget, gpointer data);
static void dialog_about_callback (GtkWidget *widget, gpointer data);
static void dialog_tree_changed (GtkTreeSelection *tree, gpointer data);
static void designer_tree_callback (GtkTreeSelection *tree, gpointer data);
static void dialog_response (GtkWidget *widget, gint response_id, gpointer data);

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

static GimpRunMode run_mode;
static gint32 image_id;
static gint32 layer_id;
static GimpDrawable *output_drawable;

static gint tile_width, tile_height;
gint sel_x1, sel_y1, sel_x2, sel_y2;
gint sel_width, sel_height;

static long num_pixels_requested = 0;

//static int debug_tuples = 0;
//static pixel_debug_info_t pixel_debug_infos[PREVIEW_SIZE * PREVIEW_SIZE];

GtkSourceBuffer *source_buffer;
GtkSourceMarker *source_marker;
GtkWidget *expression_entry = 0,
    *animation_table,
    *frame_table,
    *edge_color_x_well,
    *edge_color_y_well,
    *uservalues_scrolled_window,
    *uservalues_table,
    *tree_scrolled_window,
    *designer_widget;

int previewing = 0, auto_preview = 1, fast_preview = 1;
int expression_changed = 1;
color_t gradient_samples[USER_GRADIENT_POINTS];
int output_bpp;
int edge_behaviour_x_mode = EDGE_BEHAVIOUR_COLOR;
int edge_behaviour_y_mode = EDGE_BEHAVIOUR_COLOR;

int fast_image_source_scale;

static GimpRGB edge_color_x = { 0.0, 0.0, 0.0, 0.0 };
static GimpRGB edge_color_y = { 0.0, 0.0, 0.0, 0.0 };

mathmap_t *mathmap = 0;
mathmap_invocation_t *invocation = 0;

static char *current_filename = 0;

/***** Functions *****/

/*****/

static void
expression_copy (gchar *dest, const gchar *src)
{
    assert(strlen(src) < MAX_EXPRESSION_LENGTH);
    strcpy(dest, src);
}

static void
set_current_filename (const char *new_filename)
{
    if (current_filename != NULL)
	g_free(current_filename);

    if (new_filename == NULL)
	current_filename = NULL;
    else
	current_filename = g_strdup(new_filename);
}

static void
set_filter_source (const char *source, const char *path)
{
    set_current_filename(path);

    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(source_buffer), source, strlen(source));

    expression_copy(mmvals.expression, source);
}

/*****/

static gint
my_gimp_main (const GimpPlugInInfo *info, int argc, char *argv[])
{
    int i;

    gsl_set_error_handler_off();

    for (i = 0; i < USER_GRADIENT_POINTS; ++i)
    {
	float v = (float)i / (float)(USER_GRADIENT_POINTS - 1);

	gradient_samples[i] = MAKE_RGBA_COLOR_FLOAT(v, v, v, 1.0);
    }

#ifdef MATHMAP_CMDLINE
    for (i = 1; i < argc; ++i)
	if (strcmp(argv[i], "-gimp") == 0)
	{
	    cmd_line_mode = 0;
	    return gimp_main(info, argc, argv);
	}

    cmd_line_mode = 1;
    return cmdline_main(argc, argv);
#else
    cmd_line_mode = 0;
    return gimp_main(info, argc, argv);
#endif
}

#define gimp_main my_gimp_main
MAIN()
#undef gimp_main

/*****/

static char*
get_rc_file_name (char *name, int global)
{
    gchar *mathmap_name = (name == 0) ? "mathmap" : g_strconcat("mathmap", G_DIR_SEPARATOR_S, name, NULL);
    gchar *filename;

    assert(mathmap_name != 0);

    if (global)
	filename = g_strconcat(gimp_data_directory(), G_DIR_SEPARATOR_S, mathmap_name, NULL);
    else
	filename = gimp_personal_rc_file(mathmap_name);

    assert(filename != 0);

    if (name != 0)
	g_free(mathmap_name);

    return filename;
}

static char*
lookup_rc_file (char *name)
{
    gchar *filename;

    filename = get_rc_file_name(name, 0);

    if (!g_file_test(filename, G_FILE_TEST_EXISTS))
    {
	g_free(filename);

	filename = get_rc_file_name(name, 1);

	if (!g_file_test(filename, G_FILE_TEST_EXISTS))
	{
	    g_free(filename);
	    filename = 0;
	}
    }

    return filename;
}

/*****/

static expression_db_t*
read_expressions (void)
{
    static char *path_local = 0, *path_global = 0;

    expression_db_t *edb_local, *edb_global;

    if (path_local == 0)
    {
	path_local = get_rc_file_name(EXPRESSIONS_DIR, 0);
	path_global = get_rc_file_name(EXPRESSIONS_DIR, 1);
    }

    edb_local = read_expression_db(path_local);
    edb_global = read_expression_db(path_global);

    edb_global = merge_expression_dbs(edb_global, edb_local);

    free_expression_db(edb_local);

    return edb_global;
}

static void
register_expression_db (expression_db_t *edb, char *symbol_prefix, char *menu_prefix)
{
    int symbol_prefix_len = strlen(symbol_prefix);
    int menu_prefix_len = strlen(menu_prefix);

    for (; edb != 0; edb = edb->next)
    {
	char *symbol, *menu;
	int i;
	int name_len;

	name_len = strlen(edb->name);

	symbol = g_malloc(symbol_prefix_len + name_len + 2);
	sprintf(symbol, "%s_%s", symbol_prefix, edb->name);

	menu = g_malloc(menu_prefix_len + name_len + 2);
	sprintf(menu, "%s/%s", menu_prefix, edb->name);

	for (i = symbol_prefix_len + 1; i < symbol_prefix_len + 1 + name_len; ++i)
	    if (symbol[i] == ' ')
		symbol[i] = '_';
	    else
		symbol[i] = tolower(symbol[i]);

	if (edb->kind == EXPRESSION_DB_GROUP)
	    register_expression_db(edb->v.group.subs, symbol, menu);
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

#ifdef DEBUG_OUTPUT
	    fprintf(stderr, "registering %s (%s)\n", symbol, menu);
#endif

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
    expression_db_t *edb = read_expressions();

    if (edb == 0)
	return;

    register_expression_db(edb, "mathmap", "<Image>/Filters/Generic/MathMap");
    free_expression_db(edb);
}

static char*
expression_for_symbol (const char *symbol, expression_db_t *edb)
{
    for (; edb != 0; edb = edb->next)
    {
	int i;
	int name_len;
	int is_group = edb->kind == EXPRESSION_DB_GROUP;

	name_len = strlen(edb->name);

	if (name_len > strlen(symbol))
	    continue;
	if ((!is_group && name_len != strlen(symbol))
	    || (is_group && name_len == strlen(symbol)))
	    continue;
	if (is_group && symbol[name_len] != '_')
	    continue;

	for (i = 0; i < name_len; ++i)
	    if ((edb->name[i] == ' ' && symbol[i] != '_')
		|| (edb->name[i] != ' ' && symbol[i] != tolower(edb->name[i])))
		break;

	if (i == name_len)
	{
	    if (is_group)
	    {
		char *exp = expression_for_symbol(symbol + name_len + 1, edb->v.group.subs);

		if (exp != 0)
		    return exp;
	    }
	    else
		return read_expression(edb->v.expression.path);
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

static void
run (const gchar *name, gint nparams, const GimpParam *param, gint *nreturn_vals, GimpParam **return_vals)
{
    static GimpParam values[1];

    GimpPDBStatusType status;
    int mutable_expression = 1;
    input_drawable_t *drawable;
    GimpDrawable *gimp_drawable;
    int default_preview_width, default_preview_height;

    INIT_LOCALE("mathmap");

    if (strncmp(name, "mathmap_", 8) == 0)
    {
	char *exp = expression_for_symbol(name + 8, read_expressions());

	fprintf(stderr, "found %s\n", exp);

	if (exp != 0)
	{
	    expression_copy(mmvals.expression, exp);
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
    gimp_drawable_mask_bounds(param[2].data.d_drawable, &sel_x1, &sel_y1, &sel_x2, &sel_y2);

    sel_width = sel_x2 - sel_x1;
    sel_height = sel_y2 - sel_y1;

    tile_width = gimp_tile_width();
    tile_height = gimp_tile_height();

    /* Calculate preview size */
    calc_preview_size(DEFAULT_PREVIEW_SIZE, DEFAULT_PREVIEW_SIZE,
		      &default_preview_width, &default_preview_height);

    /* Calculate fast image source scaling factor */
    fast_image_source_scale = MAX(sel_width / default_preview_width, 1);
    assert (fast_image_source_scale >= 1);

    /* Allocate drawable structure */
    drawable = alloc_gimp_input_drawable(gimp_drawable_get(param[2].data.d_drawable));
    assert(drawable != 0);

    drawable->v.gimp.has_selection = TRUE;
    output_bpp = drawable->v.gimp.bpp;

    gimp_drawable = get_gimp_input_drawable(drawable);

    /* Init MathMap engine */
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

		if (!generate_code(0, 0))
		    status = GIMP_PDB_CALLING_ERROR;
	    }

	    break;

	case GIMP_RUN_WITH_LAST_VALS:
	    /* Possibly retrieve data */

	    gimp_get_data(name, &mmvals);

	    if (!generate_code(0, 0))
		status = GIMP_PDB_CALLING_ERROR;

	    break;

	default:
	    break;
    } /* switch */

    /* Mathmap the image */

    if ((status == GIMP_PDB_SUCCESS)
	&& (gimp_drawable_is_rgb(GIMP_DRAWABLE_ID(gimp_drawable))
	    || gimp_drawable_is_gray(GIMP_DRAWABLE_ID(gimp_drawable))))
    {
	int animation_enabled = mmvals.flags & FLAG_ANIMATION;

	update_gradient();

	/* Set the tile cache size */
	gimp_tile_cache_ntiles((gimp_drawable->width + gimp_tile_width() - 1)
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
		gimp_image_add_layer(image_id, layer, 0);
		do_mathmap(frame, t);
	    }
	    gimp_image_undo_group_end(image_id);
	}
	else
	{
	    output_drawable = gimp_drawable;
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

    gimp_drawable_detach(gimp_drawable);

#ifdef DEBUG_OUTPUT
    g_print("%ld pixels requested\n", num_pixels_requested);
#endif
} /* run */

/*****/

static expression_db_t*
get_designer_edb (void)
{
    static expression_db_t *edb = NULL;

    if (edb == NULL)
	edb = read_expressions();

    return edb;
}

static designer_design_t*
get_current_design (void)
{
    static designer_design_type_t *the_design_type = NULL;
    static designer_design_t *the_design = NULL;

    if (the_design_type == NULL)
	the_design_type = design_type_from_expression_db(get_designer_edb());

    if (the_design == NULL)
	the_design = designer_make_design(the_design_type);

    return the_design;
}

static void
node_focussed_callback (GtkWidget *widget, designer_node_t *node)
{
    char *source;

    source = make_filter_source_from_designer_node(node, "__composer_filter__");

    set_filter_source(source, NULL);

    g_free(source);
}

static void
design_changed_callback (GtkWidget *widget, designer_design_t *design)
{
    designer_node_t *node = designer_widget_get_focussed_node(widget);

    g_print("design changed\n");

    if (node != NULL)
	node_focussed_callback(widget, node);
}

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
    int i, num_samples;
    gchar *gradient_name;

    gradient_name = gimp_context_get_gradient();
    assert(gradient_name != 0);

    gimp_gradient_get_uniform_samples(gradient_name, USER_GRADIENT_POINTS, FALSE, &num_samples, &samples);

    for (i = 0; i < num_samples / 4; ++i)
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
	char *template_filename;

	if (run_mode == GIMP_RUN_INTERACTIVE && expression_entry != 0)
	    dialog_text_update();

	if (mathmap != 0)
	    unload_mathmap(mathmap);

	template_filename = lookup_rc_file(MAIN_TEMPLATE_FILENAME);
	if (template_filename == 0)
	{
	    sprintf(error_string, "Cannot find template file `%s'.  MathMap is not installed correctly.", MAIN_TEMPLATE_FILENAME);
	    new_mathmap = 0;
	}
	else
	{
	    char *opmacros_name = lookup_rc_file(OPMACROS_FILENAME);

	    if (opmacros_name == 0)
	    {
		sprintf(error_string, "Support file `%s' does not exist.  MathMap is not installed correctly.", OPMACROS_FILENAME);
		new_mathmap = 0;
	    }
	    else
	    {
		char *include_path = g_path_get_dirname(opmacros_name);

		new_mathmap = compile_mathmap(mmvals.expression, template_filename, include_path);

		g_free(include_path);
	    }
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
	    /*
	    new_invocation->origin_x = sel_x1;
	    new_invocation->origin_y = sel_y1;
	    */

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

	if (animation_table != 0)
	    gtk_widget_set_sensitive(GTK_WIDGET(animation_table), mathmap != 0 && does_filter_use_t(mathmap->main_filter));
    }

    if (invocation != 0)
    {
	invocation->antialiasing = mmvals.flags & FLAG_ANTIALIASING;
	invocation->supersampling = mmvals.flags & FLAG_SUPERSAMPLING;

	invocation->current_frame = current_frame;
	invocation->current_t = current_t;

	invocation->edge_behaviour_x = edge_behaviour_x_mode;
	invocation->edge_behaviour_y = edge_behaviour_y_mode;
	invocation->edge_color_x = MAKE_RGBA_COLOR_FLOAT(edge_color_x.r, edge_color_x.g, edge_color_x.b, edge_color_x.a);
	invocation->edge_color_y = MAKE_RGBA_COLOR_FLOAT(edge_color_y.r, edge_color_y.g, edge_color_y.b, edge_color_y.a);

	update_image_internals(invocation);
    }

    return invocation != 0;
}

/*****/

static void
unref_drawable_tiles (input_drawable_t *drawable)
{
    g_assert(drawable->kind == INPUT_DRAWABLE_GIMP);

    if (drawable->v.gimp.tile != 0)
    {
	gimp_tile_unref(drawable->v.gimp.tile, FALSE);
	drawable->v.gimp.tile = 0;
    }
}

static void
unref_tiles (void)
{
    for_each_input_drawable(unref_drawable_tiles);
}

input_drawable_t*
alloc_gimp_input_drawable (GimpDrawable *gimp_drawable)
{
    int width = gimp_drawable_width(GIMP_DRAWABLE_ID(gimp_drawable));
    int height = gimp_drawable_height(GIMP_DRAWABLE_ID(gimp_drawable));
    input_drawable_t *drawable = alloc_input_drawable(INPUT_DRAWABLE_GIMP, width, height);

    drawable->v.gimp.drawable = gimp_drawable;
    drawable->v.gimp.bpp = gimp_drawable_bpp(GIMP_DRAWABLE_ID(gimp_drawable));
    drawable->v.gimp.row = -1;
    drawable->v.gimp.col = -1;
    drawable->v.gimp.tile = 0;
    drawable->v.gimp.fast_image_source = 0;
    drawable->v.gimp.has_selection = FALSE;

    drawable->v.gimp.fast_image_source_width = drawable->width / fast_image_source_scale;
    drawable->v.gimp.fast_image_source_height = drawable->height / fast_image_source_scale;

    return drawable;
}

GimpDrawable*
get_gimp_input_drawable (input_drawable_t *drawable)
{
    g_assert(drawable->kind == INPUT_DRAWABLE_GIMP);

    return drawable->v.gimp.drawable;
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
	    int region_x = dest_rgn.x - sel_x1;
	    int region_y = dest_rgn.y - sel_y1;
	    int region_width = dest_rgn.w;
	    int region_height = dest_rgn.h;

	    invocation->row_stride = dest_rgn.rowstride;
	    invocation->output_bpp = gimp_drawable_bpp(GIMP_DRAWABLE_ID(output_drawable));

	    call_invocation_parallel_and_join(invocation, region_x, region_y, region_width, region_height,
					      dest_rgn.data, 1);

	    /* Update progress */
	    progress += region_width * region_height;
	    gimp_progress_update((double) progress / max_progress);
	}

	unref_tiles();

	gimp_drawable_flush(output_drawable);
	gimp_drawable_merge_shadow(GIMP_DRAWABLE_ID(output_drawable), TRUE);
	gimp_drawable_update(GIMP_DRAWABLE_ID(output_drawable), sel_x1, sel_y1, sel_width, sel_height);
    }
} /* mathmap */

/*****/

static color_t
get_pixel (mathmap_invocation_t *invocation, input_drawable_t *drawable, int frame, int x, int y)
{
    gint newcol, newrow;
    gint newcoloff, newrowoff;
    guchar *p;
    guchar r, g, b, a;
    int bpp;

    ++num_pixels_requested;

    if (x < 0 || x >= drawable->width)
	return invocation->edge_color_x;
    if (y < 0 || y >= drawable->height)
	return invocation->edge_color_y;

#ifdef MATHMAP_CMDLINE
    if (cmd_line_mode)
	return cmdline_mathmap_get_pixel(invocation, drawable, frame, x, y);
#endif

    newcol = x / tile_width;
    newcoloff = x % tile_width;
    newrow = y / tile_height;
    newrowoff = y % tile_height;

    if (drawable->v.gimp.col != newcol || drawable->v.gimp.row != newrow || drawable->v.gimp.tile == NULL)
    {
	if (drawable->v.gimp.tile != NULL)
	    gimp_tile_unref(drawable->v.gimp.tile, FALSE);

	drawable->v.gimp.tile = gimp_drawable_get_tile(drawable->v.gimp.drawable, FALSE, newrow, newcol);
	assert(drawable->v.gimp.tile != 0);
	gimp_tile_ref(drawable->v.gimp.tile);

	drawable->v.gimp.col = newcol;
	drawable->v.gimp.row = newrow;
    }

    p = drawable->v.gimp.tile->data + drawable->v.gimp.tile->bpp * (drawable->v.gimp.tile->ewidth * newrowoff + newcoloff);

    bpp = drawable->v.gimp.bpp;

    if (bpp == 1 || bpp == 2)
	r = g = b = p[0];
    else if (bpp == 3 || bpp == 4)
    {
	r = p[0];
	g = p[1];
	b = p[2];
    }
    else
	assert(0);

    if (bpp == 1 || bpp == 3)
	a = 255;
    else
	a = p[bpp - 1];

    return MAKE_RGBA_COLOR(r, g, b, a);
}

static void
build_fast_image_source (input_drawable_t *drawable)
{
    color_t *p;
    int width, height;
    int x, y;
    int img_x1, img_y1, img_width, img_height;

    if (drawable->v.gimp.fast_image_source != 0)
	return;

    width = drawable->v.gimp.fast_image_source_width;
    height = drawable->v.gimp.fast_image_source_height;

    p = drawable->v.gimp.fast_image_source = g_malloc(width * height * sizeof(color_t));

    if (drawable->v.gimp.has_selection)
    {
	img_x1 = sel_x1;
	img_y1 = sel_y1;
	img_width = sel_width;
	img_height = sel_height;
    }
    else
    {
	img_x1 = img_y1 = 0;
	img_width = drawable->width;
	img_height = drawable->height;
    }

    for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	    drawable->v.gimp.fast_image_source[x + y * width] =
		get_pixel(invocation, drawable, 0,
			  img_x1 + x * img_width / width,
			  img_y1 + y * img_height / height);
}

static color_t
get_pixel_fast (mathmap_invocation_t *invocation, input_drawable_t *drawable, int x, int y)
{
    int x1, y1;

    if (drawable->v.gimp.has_selection)
    {
	x1 = sel_x1;
	y1 = sel_y1;
    }
    else
    {
	x1 = 0;
	y1 = 0;
    }

    x = (x - x1) / fast_image_source_scale;
    y = (y - y1) / fast_image_source_scale;

    if (x < 0 || x >= drawable->v.gimp.fast_image_source_width)
	return invocation->edge_color_x;
    if (y < 0 || y >= drawable->v.gimp.fast_image_source_height)
	return invocation->edge_color_y;

    g_assert(drawable->v.gimp.fast_image_source != 0);

    return drawable->v.gimp.fast_image_source[x + y * drawable->v.gimp.fast_image_source_width];
}

color_t
mathmap_get_pixel (mathmap_invocation_t *invocation, input_drawable_t *drawable, int frame, int x, int y)
{
    g_assert(drawable != 0);

    if (previewing)
	return get_pixel_fast(invocation, drawable, x, y);
    else
	return get_pixel(invocation, drawable, frame, x, y);
}

void
drawable_get_pixel_inc (mathmap_invocation_t *invocation, input_drawable_t *drawable, int *inc_x, int *inc_y)
{
    if (previewing)
	*inc_x = *inc_y = fast_image_source_scale;
    else
	*inc_x = *inc_y = 1;
}

/*****/

static void
tree_from_expression_db (GtkTreeStore *store, GtkTreeIter *parent, expression_db_t *edb)
{
    for (; edb != 0; edb = edb->next)
    {
	GtkTreeIter iter;

	if (edb->kind == EXPRESSION_DB_GROUP)
	{
	    gtk_tree_store_append(store, &iter, parent);
	    gtk_tree_store_set(store, &iter, 0, edb->name, -1);

	    tree_from_expression_db(store, &iter, edb->v.group.subs);
	}
	else if (edb->kind == EXPRESSION_DB_EXPRESSION)
	{
	    gtk_tree_store_append(store, &iter, parent);
	    gtk_tree_store_set(store, &iter,
			       0, edb->name,
			       1, edb->v.expression.path,
			       2, get_expression_name(edb),
			       -1);
	}
	else
	    assert(0);
    }
}

static GtkWidget*
make_tree_from_edb (expression_db_t *edb, GCallback callback)
{
    GtkTreeStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    GtkWidget *tree;

    /* model */
    store = gtk_tree_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

    if (edb != 0)
    	tree_from_expression_db(store, NULL, edb);

    /* view */
    tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);
    g_signal_connect(G_OBJECT(selection), "changed",
		     G_CALLBACK(callback),
		     (gpointer)NULL);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("Examples", renderer,
		    				      "text", 0,
						      NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

    gtk_widget_show(tree);

    return tree;
}

/*****/

static void
update_userval_table (void)
{
    if (uservalues_table != 0)
	gtk_container_remove(GTK_CONTAINER(GTK_BIN(uservalues_scrolled_window)->child), uservalues_table);

    uservalues_table = make_userval_table(mathmap->main_filter->userval_infos, invocation->uservals);

    if (uservalues_table != 0)
    {
#if GTK_MAJOR_VERSION < 1 || (GTK_MAJOR_VERSION == 1 && GTK_MINOR_VERSION < 1)
	gtk_container_add(GTK_CONTAINER(uservalues_scrolled_window), uservalues_table);
#else
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(uservalues_scrolled_window), uservalues_table);
#endif
    }
}

static void
update_expression_tree_from_edb (GtkWidget *tree_scrolled_window, expression_db_t *edb, GCallback callback)
{
    GtkWidget *tree;

    if (gtk_bin_get_child(GTK_BIN(tree_scrolled_window)) != 0)
	gtk_container_remove(GTK_CONTAINER(tree_scrolled_window), gtk_bin_get_child(GTK_BIN(tree_scrolled_window)));

    tree = make_tree_from_edb(edb, callback);

    gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(tree_scrolled_window), tree);
    gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(tree)), GTK_SELECTION_BROWSE);
    gtk_widget_show(tree);
}

static void
update_expression_tree (void)
{
    expression_db_t *edb = read_expressions();

    update_expression_tree_from_edb(tree_scrolled_window, edb, G_CALLBACK(dialog_tree_changed));

    free_expression_db(edb);
}

/*****/

#define RESPONSE_ABOUT 1

static GtkWidget*
make_edge_behaviour_frame (char *name, int direction_flag, GtkWidget **edge_color_well, GimpRGB *edge_color)
{
    GtkWidget *table, *frame, *toggle;
    GSList *edge_group = 0;

    table = gtk_table_new(2, 4, FALSE);
    gtk_container_border_width(GTK_CONTAINER(table), 6);
    gtk_table_set_row_spacings(GTK_TABLE(table), 4);

    frame = gtk_frame_new(name);
    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
    gtk_container_add(GTK_CONTAINER(frame), table);

    gtk_widget_show(table);
    gtk_widget_show(frame);

    /* Color */

    toggle = gtk_radio_button_new_with_label(edge_group, _("Color"));
    edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
    gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
    gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
		       (GtkSignalFunc)dialog_edge_behaviour_update, GINT_TO_POINTER(EDGE_BEHAVIOUR_COLOR | direction_flag));
    gtk_widget_show(toggle);

    *edge_color_well = gimp_color_button_new(_("Edge Color"), 32, 16,
					     edge_color, GIMP_COLOR_AREA_SMALL_CHECKS);
    gtk_signal_connect(GTK_OBJECT(*edge_color_well), "color_changed",
		       (GtkSignalFunc)dialog_edge_color_changed, GINT_TO_POINTER(direction_flag));
    gtk_widget_show(*edge_color_well);
    gtk_table_attach(GTK_TABLE(table), *edge_color_well, 1, 2, 0, 1, GTK_FILL, 0, 0, 0);

    /* Wrap */

    toggle = gtk_radio_button_new_with_label(edge_group, _("Wrap"));
    edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
    gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);
    gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
		       (GtkSignalFunc)dialog_edge_behaviour_update, GINT_TO_POINTER(EDGE_BEHAVIOUR_WRAP | direction_flag));
    gtk_widget_show(toggle);

    /* Reflect */

    toggle = gtk_radio_button_new_with_label(edge_group, _("Reflect"));
    edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
    gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);
    gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
		       (GtkSignalFunc)dialog_edge_behaviour_update, GINT_TO_POINTER(EDGE_BEHAVIOUR_REFLECT | direction_flag));
    gtk_widget_show(toggle);

    /* Rotate */

    toggle = gtk_radio_button_new_with_label(edge_group, _("Rotate"));
    edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
    gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 3, 4, GTK_FILL, 0, 0, 0);
    gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
		       (GtkSignalFunc)dialog_edge_behaviour_update, GINT_TO_POINTER(EDGE_BEHAVIOUR_ROTATE | direction_flag));
    gtk_widget_show(toggle);

    return frame;
}

static GtkWidget*
make_tree_scrolled_window (void)
{
    GtkWidget *tree_scrolled_window;

    tree_scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(tree_scrolled_window),
				    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_show (tree_scrolled_window);

    return tree_scrolled_window;
}

#define ERROR_PIXMAP_NAME	PIXMAP_DIR "/error.png"

static gint
mathmap_dialog (int mutable_expression)
{
    GtkWidget *dialog;
    GtkWidget *top_table, *middle_table;
    GtkWidget *hpaned;
    GtkWidget *vbox;
    GtkWidget *frame;
    GtkWidget *table;
    GtkWidget *button;
    GtkWidget *label;
    GtkWidget *toggle;
    GtkWidget *alignment;
    GtkWidget *scale;
    GtkWidget *notebook;
    GtkWidget *t_table;
    GtkWidget *designer_tree_scrolled_window;
    GtkObject *adjustment;
    GdkPixbuf *pixbuf;

    gimp_ui_init("mathmap", TRUE);

    alloc_preview_pixbuf(DEFAULT_PREVIEW_SIZE, DEFAULT_PREVIEW_SIZE);

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

    top_table = gtk_hpaned_new();
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), top_table, TRUE, TRUE, 0);
    gtk_widget_show(top_table);

    /* Preview */

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_paned_add1(GTK_PANED(top_table), vbox);
    gtk_widget_show(vbox);

    wint.preview = gimp_preview_area_new();
    gtk_widget_set_size_request(wint.preview, gdk_pixbuf_get_width(wint.pixbuf), gdk_pixbuf_get_height(wint.pixbuf));
    gtk_signal_connect(GTK_OBJECT(wint.preview), "size-allocate",
		       G_CALLBACK(dialog_preview_size_allocate), NULL);

    /*
    gtk_widget_add_events(GTK_WIDGET(wint.preview), GDK_BUTTON_PRESS_MASK);
    gtk_signal_connect (GTK_OBJECT (wint.preview), "button-press-event",
			(GtkSignalFunc)dialog_preview_click, 0);
    */

    gtk_box_pack_start(GTK_BOX(vbox), wint.preview, TRUE, TRUE, 0);
    gtk_widget_show(wint.preview);

    button = gtk_button_new_with_label(_("Preview"));
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       (GtkSignalFunc)dialog_preview_callback, 0);
    gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
    gtk_widget_show(button);

    /* Notebook */

    notebook = gtk_notebook_new();
    gtk_notebook_set_tab_pos (GTK_NOTEBOOK (notebook), GTK_POS_TOP);
    gtk_paned_add2(GTK_PANED(top_table), notebook);
    gtk_widget_show(notebook);

        /* Expression */

	if (mutable_expression)
	{
	    PangoFontDescription *font_desc;
	    GtkWidget *scrolled_window;
	    GtkSourceLanguagesManager *manager;
	    GtkSourceLanguage *language;

	    table = gtk_table_new(2, 2, FALSE);
	    gtk_container_border_width(GTK_CONTAINER(table), 0);
	    gtk_table_set_col_spacings(GTK_TABLE(table), 4);
	    gtk_widget_show(table);

	    /* Language */
	    manager = gtk_source_languages_manager_new();
	    language = gtk_source_languages_manager_get_language_from_mime_type(manager, "application/x-mathmap");

	    /* Source Buffer */
	    source_buffer = gtk_source_buffer_new(NULL);
	    if (language != NULL)
		gtk_source_buffer_set_language(source_buffer, language);
	    gtk_source_buffer_set_highlight(source_buffer, TRUE);

	    /* Scrolled Window */
	    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
					   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	    gtk_widget_show(scrolled_window);

	    /* Source View */
	    expression_entry = gtk_source_view_new_with_buffer(source_buffer);
	    gtk_widget_show(expression_entry);

	    gtk_source_view_set_show_line_markers(GTK_SOURCE_VIEW(expression_entry), TRUE);

	    if ((pixbuf = gdk_pixbuf_new_from_file(ERROR_PIXMAP_NAME, NULL)))
	    {
		gtk_source_view_set_marker_pixbuf (GTK_SOURCE_VIEW (expression_entry), "one", pixbuf);
		g_object_unref (pixbuf);
	    }
	    else
		g_warning("Could not find image file `%s'.", ERROR_PIXMAP_NAME);

	    gtk_container_add(GTK_CONTAINER(scrolled_window), expression_entry);

	    gtk_table_attach(GTK_TABLE(table), scrolled_window, 0, 2, 0, 1,
			     GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND, 0, 0);

	    g_signal_connect(G_OBJECT(source_buffer), "changed",
			     G_CALLBACK(dialog_text_changed),
			     (gpointer)NULL);

	    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(source_buffer), mmvals.expression,
			    	     strlen(mmvals.expression));

	    font_desc = pango_font_description_from_string("Courier 10");
	    if (font_desc != NULL)
	    {
		gtk_widget_modify_font(expression_entry, font_desc);
		pango_font_description_free(font_desc);
	    }

	    button = gtk_button_new_with_label(_("Save"));
	    gtk_signal_connect(GTK_OBJECT(button), "clicked", (GtkSignalFunc)dialog_save_callback, 0);
	    gtk_table_attach(GTK_TABLE(table), button, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);
	    gtk_widget_show(button);

	    button = gtk_button_new_with_label(_("Save As..."));
	    gtk_signal_connect(GTK_OBJECT(button), "clicked", (GtkSignalFunc)dialog_save_as_callback, 0);
	    gtk_table_attach(GTK_TABLE(table), button, 1, 2, 1, 2, GTK_FILL, 0, 0, 0);
	    gtk_widget_show(button);

	    label = gtk_label_new(_("Expression"));
	    gtk_widget_show(label);
	    gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook),
			    		  table, label, label);
	}

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

	    frame = make_edge_behaviour_frame(_("Edge Behaviour X"), EDGE_BEHAVIOUR_X_FLAG, &edge_color_x_well, &edge_color_x);
	    gtk_table_attach(GTK_TABLE(middle_table), frame, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);

	    frame = make_edge_behaviour_frame(_("Edge Behaviour Y"), EDGE_BEHAVIOUR_Y_FLAG, &edge_color_y_well, &edge_color_y);
	    gtk_table_attach(GTK_TABLE(middle_table), frame, 1, 2, 2, 3, GTK_FILL, 0, 0, 0);

	    /* Animation */

	    animation_table = gtk_table_new(4, 1, FALSE);
	    gtk_container_border_width(GTK_CONTAINER(animation_table), 6);
	    gtk_table_set_row_spacings(GTK_TABLE(animation_table), 4);
	    gtk_table_set_col_spacings(GTK_TABLE(animation_table), 4);
	    gtk_widget_set_sensitive(GTK_WIDGET(animation_table), FALSE);
    
	    frame = gtk_frame_new(NULL);
	    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
	    gtk_container_add(GTK_CONTAINER(frame), animation_table);

	    alignment = gtk_alignment_new(0, 0, 0, 0);
	    gtk_container_add(GTK_CONTAINER(alignment), frame);
	    gtk_table_attach(GTK_TABLE(middle_table), alignment, 1, 2, 0, 2, GTK_FILL, 0, 0, 0);

	    gtk_widget_show(animation_table);
	    gtk_widget_show(frame);
	    gtk_widget_show(alignment);

	        /* Animation Toggle */

	        alignment = gtk_alignment_new(0, 0, 0, 0);
		toggle = gtk_check_button_new_with_label(_("Animate"));
		gtk_container_add(GTK_CONTAINER(alignment), toggle);
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle),
					    mmvals.flags & FLAG_ANIMATION);
		gtk_table_attach(GTK_TABLE(animation_table), alignment, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_animation_update, 0);
		gtk_widget_show(toggle);
		gtk_widget_show(alignment);

		/* Number of Frames */

		frame_table = gtk_table_new(1, 2, FALSE);
		gtk_table_set_col_spacings(GTK_TABLE(frame_table), 4);
		gtk_table_attach(GTK_TABLE(animation_table), frame_table, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);

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
		gtk_table_attach(GTK_TABLE(animation_table), alignment, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_periodic_update, 0);
		gtk_widget_show(toggle);
		gtk_widget_show(alignment);

		/* t */

		t_table = gtk_table_new(1, 2, FALSE);
		gtk_table_set_col_spacings(GTK_TABLE(t_table), 4);
		gtk_table_attach(GTK_TABLE(animation_table), t_table, 0, 1, 4, 5, GTK_FILL, 0, 0, 0);

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

	label = gtk_label_new(_("Settings"));
	gtk_widget_show(label);
	gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), middle_table, label, label);

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
	    tree_scrolled_window = make_tree_scrolled_window();

	    update_expression_tree();

	    label = gtk_label_new(_("Examples"));
	    gtk_widget_show(label);
	    gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), tree_scrolled_window, label, label);
	}

	/* Designer */

	hpaned = gtk_hpaned_new();

	designer_tree_scrolled_window = make_tree_scrolled_window();
	update_expression_tree_from_edb(designer_tree_scrolled_window, get_designer_edb(),
					G_CALLBACK(designer_tree_callback));
	gtk_paned_add1(GTK_PANED(hpaned), designer_tree_scrolled_window);

	designer_widget = designer_widget_new(get_current_design(), design_changed_callback, node_focussed_callback);
	gtk_paned_add2(GTK_PANED(hpaned), designer_widget);

	gtk_widget_show(hpaned);

	label = gtk_label_new(_("Composer"));
	gtk_widget_show(label);
	gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), hpaned, label, label);

    /* Done */

    if (!mutable_expression)
	dialog_update_preview();

    gtk_widget_show(dialog);

    gtk_main();
    gdk_flush();

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

static gboolean
recalculate_preview (void)
{
    static int in_recalculate = 0;

    if (in_recalculate > 0)
	return FALSE;

    ++in_recalculate;

    if (generate_code(0, mmvals.param_t))
    {
	int preview_width = gdk_pixbuf_get_width(wint.pixbuf);
	int preview_height = gdk_pixbuf_get_height(wint.pixbuf);
	int x, y;
	guchar *p_ul, *p;
	gint check, check_0, check_1; 
	guchar *buf = (guchar*)malloc(4 * preview_width * preview_height);
	float old_scale_x, old_scale_y;

	assert(buf != 0);

	update_uservals(mathmap->main_filter->userval_infos, invocation->uservals);

	previewing = fast_preview;

	invocation->row_stride = preview_width * 4;
	invocation->output_bpp = 4;

	/*
	if (debug_tuples)
	{
	    int i;

	    enable_debugging(invocation);
	    for (i = 0; i < PREVIEW_SIZE * PREVIEW_SIZE; ++i)
		pixel_debug_infos[i].num_debug_tuples = 0;
	}
	else
	*/
	    disable_debugging(invocation);

	old_scale_x = invocation->scale_x;
	old_scale_y = invocation->scale_y;

	invocation->scale_x *= (float)sel_width / (float)preview_width;
	invocation->scale_y *= (float)sel_height / (float)preview_height;

	if (previewing) {
	    for_each_input_drawable(build_fast_image_source);
	    call_invocation_parallel_and_join(invocation, 0, 0, preview_width, preview_height, buf, get_num_cpus());
	}
	else
	    call_invocation_parallel_and_join(invocation, 0, 0, preview_width, preview_height, buf, 1);

	invocation->scale_x = old_scale_x;
	invocation->scale_y = old_scale_y;

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

	--in_recalculate;

	return TRUE;
    }

    --in_recalculate;

    return FALSE;
}

static void
dialog_update_preview (void)
{
    if (recalculate_preview())
    {
	refresh_preview();
	gtk_widget_draw(wint.preview, NULL);
    }
}

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

static void
dialog_text_changed (GtkTextBuffer *buffer, gpointer user_data)
{
    expression_changed = 1;
}

/*****/

static void
dialog_text_update (void)
{
    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(expression_entry));
    GtkTextIter start, end;
    gchar *expression;

    gtk_text_buffer_get_bounds(buffer, &start, &end);
    expression = gtk_text_buffer_get_text(buffer, &start, &end, TRUE);

    expression_copy(mmvals.expression, expression);

    g_free(expression);
}

void
delete_expression_marker (void)
{
    if (source_marker != 0)
    {
	gtk_source_buffer_delete_marker(source_buffer, source_marker);
	source_marker = 0;
    }
}

void
set_expression_marker (int line, int column)
{
    delete_expression_marker();

    if (expression_entry != 0)
    {
	GtkTextIter iter;

	g_print("line %d\n", line);

	gtk_text_buffer_get_iter_at_line_index(GTK_TEXT_BUFFER(source_buffer), &iter, line, 0);
	source_marker = gtk_source_buffer_create_marker(source_buffer, NULL, "one", &iter);
    }
}

/*****/

static void
dialog_supersampling_update (GtkWidget *widget, gpointer data)
{
    mmvals.flags &= ~FLAG_SUPERSAMPLING;

    if (GTK_TOGGLE_BUTTON(widget)->active)
	mmvals.flags |= FLAG_SUPERSAMPLING;

    if (auto_preview)
	dialog_update_preview();
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
dialog_edge_behaviour_update (GtkWidget *widget, gpointer _data)
{
    int data = GPOINTER_TO_INT(_data);
    int edge_behaviour_mode = data & EDGE_BEHAVIOUR_MASK;

    if (data & EDGE_BEHAVIOUR_X_FLAG)
    {
	edge_behaviour_x_mode = edge_behaviour_mode;

	if (edge_behaviour_mode == EDGE_BEHAVIOUR_COLOR)
	    gtk_widget_set_sensitive(edge_color_x_well, 1);
	else
	    gtk_widget_set_sensitive(edge_color_x_well, 0);
    }
    else if (data & EDGE_BEHAVIOUR_Y_FLAG)
    {
	edge_behaviour_y_mode = edge_behaviour_mode;

	if (edge_behaviour_mode == EDGE_BEHAVIOUR_COLOR)
	    gtk_widget_set_sensitive(edge_color_y_well, 1);
	else
	    gtk_widget_set_sensitive(edge_color_y_well, 0);
    }
    else
	assert(0);

    if (auto_preview)
	dialog_update_preview();
}

static void
dialog_edge_color_changed (GtkWidget *color_well, gpointer data)
{
    int flag = GPOINTER_TO_INT(data);

    if (flag == EDGE_BEHAVIOUR_X_FLAG)
	gimp_color_button_get_color(GIMP_COLOR_BUTTON(color_well), &edge_color_x);
    else if (flag == EDGE_BEHAVIOUR_Y_FLAG)
	gimp_color_button_get_color(GIMP_COLOR_BUTTON(color_well), &edge_color_y);
    else
	assert(0);

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
calc_preview_size (int max_width, int max_height, int *width, int *height)
{
    int pwidth, pheight;

    pwidth  = MIN(sel_width, max_width);
    pheight = sel_height * pwidth / sel_width;

    if (pheight > max_height)
    {
	pheight = MIN(sel_height, max_height);
	pwidth  = sel_width * pheight / sel_height;
    }

    *width  = MAX(pwidth, 2);  /* Min size is 2 */
    *height = MAX(pheight, 2);
}

static gboolean
alloc_preview_pixbuf (int max_width, int max_height)
{
    int width, height;

    calc_preview_size(max_width, max_height, &width, &height);

    if (wint.pixbuf != 0)
    {
	if (gdk_pixbuf_get_width(wint.pixbuf) == width && gdk_pixbuf_get_height(wint.pixbuf) == height)
	    return FALSE;
	g_object_unref(G_OBJECT(wint.pixbuf));
    }
    if (wint.wimage != 0)
	g_free(wint.wimage);

#ifdef DEBUG_OUTPUT
    g_print("allocing pixbuf %dx%d\n", width, height);
#endif

    wint.wimage = g_malloc(width * height * 3);
    memset(wint.wimage, 0, width * height * 3);

    wint.pixbuf = gdk_pixbuf_new_from_data(wint.wimage, GDK_COLORSPACE_RGB, FALSE, 8,
					   width, height, width * 3,
					   NULL, NULL);

    return TRUE;
}

static void
refresh_preview (void)
{
    gimp_preview_area_fill(GIMP_PREVIEW_AREA(wint.preview),
			   0, 0,
			   wint.preview->allocation.width, wint.preview->allocation.height,
			   239, 235, 229);

    if (wint.wimage == 0)
	return;

    gimp_preview_area_draw(GIMP_PREVIEW_AREA(wint.preview),
			   0, 0, /* pos */
			   gdk_pixbuf_get_width(wint.pixbuf), gdk_pixbuf_get_height(wint.pixbuf), /* size */
			   GIMP_RGB_IMAGE,
			   wint.wimage,
			   gdk_pixbuf_get_width(wint.pixbuf) * 3);
}

static void
dialog_preview_size_allocate (GtkWidget *widget, GtkAllocation *allocation, gpointer user_data)
{
    //g_print("size allocation: %dx%d\n", allocation->width, allocation->height);

    if (alloc_preview_pixbuf(allocation->width, allocation->height))
    {
	if (auto_preview && !expression_changed)
	    recalculate_preview();
    }

    refresh_preview();
}

static void
dialog_preview_callback (GtkWidget *widget, gpointer data)
{
    update_gradient();
    dialog_update_preview();
}

/*****/

/*
static pixel_debug_info_t*
get_pixel_debug_info (int row, int col)
{
    assert(row >= 0 && row < PREVIEW_SIZE && col >= 0 && col < PREVIEW_SIZE);

    return &pixel_debug_infos[row * PREVIEW_SIZE + col];
}
*/

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

static void
dialog_preview_click (GtkWidget *widget, GdkEvent *event)
{
    switch (event->type)
    {
	case GDK_BUTTON_PRESS :
	    if (debug_tuples)
	    {
		GdkEventButton *bevent = (GdkEventButton*)event;
		int row = bevent->y;
		int col = bevent->x;
		pixel_debug_info_t *info = get_pixel_debug_info(row, col);
		int i;

		printf("clicked at %d %d\n", row, col);

		for (i = 0; i < info->num_debug_tuples; ++i)
		{
		    print_tuple(info->debug_tuples[i]);
		    printf("\n");
		}
	    }
	    break;

	case GDK_MOTION_NOTIFY :
	    printf("motion\n");
	    break;

	default :
	    break;
    }
}
*/

/*****/

static void
save_expression (void)
{
    FILE *file;
    size_t len;

    assert(current_filename != 0);

    dialog_text_update();

    file = fopen(current_filename, "w");
    if (file == 0)
    {
	char *message = g_strdup_printf(_("Cannot open file `%s': %m"), current_filename);

	gimp_message(message);
	g_free(message);

	return;
    }

    len = strlen(mmvals.expression);
    if (fwrite(mmvals.expression, 1, len, file) != len)
    {
	char *message = g_strdup_printf(_("Could not write to file `%s': %m"), current_filename);

	gimp_message(message);
	g_free(message);
    }

    fclose(file);
}

static void
dialog_save_as_callback (GtkWidget *widget, gpointer data)
{
    GtkWidget *dialog;

    dialog = gtk_file_chooser_dialog_new ("Save Expression",
					  NULL,
					  GTK_FILE_CHOOSER_ACTION_SAVE,
					  GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					  GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
					  NULL);
    gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);

    if (current_filename == 0)
    {
	char *mathmap_path = get_rc_file_name(0, 0);
	char *default_path = get_rc_file_name(EXPRESSIONS_DIR, 0);

	/* We try to create the directory just in case.  If it already
	   exists, nothing happens and it doesn't hurt. */
	mkdir(mathmap_path, 0777);
	mkdir(default_path, 0777);

	gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), default_path);
	gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog), "Untitled expression.mm");

	g_free(mathmap_path);
	g_free(default_path);
    }
    else
	gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (dialog), current_filename);

    if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
	char *filename;

	filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
	set_current_filename(filename);
	g_free(filename);

	save_expression();

	update_expression_tree();
    }

    gtk_widget_destroy (dialog);
}

static void
dialog_save_callback (GtkWidget *widget, gpointer data)
{
    if (current_filename == 0)
	dialog_save_as_callback(widget, data);
    else
	save_expression();
}

/*****/

static void
dialog_ok_callback (GtkWidget *widget, gpointer data)
{
    if (generate_code(0, 0))
    {
	wint.run = TRUE;
	if (!does_filter_use_t(mathmap->main_filter))
	    mmvals.flags &= ~FLAG_ANIMATION;
	gtk_widget_destroy(GTK_WIDGET(data));
    }
}

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

#define MATHMAP_PIXMAP_NAME	PIXMAP_DIR "/mathmap.png"

static void
dialog_about_callback (GtkWidget *widget, gpointer data)
{
    static GdkPixbuf *mathmap_logo = NULL;

    char *gpl = "MathMap is free software; you can redistribute it and/or modify it\n"\
	"under the terms of the GNU General Public License as published by the\n"\
	"Free Software Foundation; either version 2 of the License, or (at your\n"\
	"option) any later version.\n"\
	"\n"\
	"MathMap is distributed in the hope that it will be useful, but WITHOUT\n"\
	"ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or\n"\
	"FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License\n"\
	"for more details.\n"\
	"\n"\
	"You should have received a copy of the GNU General Public License\n"\
	"along with MathMap; if not, write to the Free Software Foundation,\n"\
	"Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.";
    char *authors[] = { "Mark Probst <schani@complang.tuwien.ac.at>", 0 };
    char *artists[] = { "Herbert Poetzl <herbert@13thfloor.at>", 0 };

    if (mathmap_logo == NULL)
	mathmap_logo = gdk_pixbuf_new_from_file(MATHMAP_PIXMAP_NAME, NULL);

    gtk_show_about_dialog (NULL, 
			   "name", "MathMap",
			   "version", MATHMAP_VERSION,
			   "authors", authors,
			   "artists", artists,
			   "comments", "An image generation and manipulation language",
			   "website", "http://www.complang.tuwien.ac.at/schani/mathmap/",
			   "copyright", "Copyright © 1997-2008 Mark Probst",
			   "license", gpl,
			   "logo", mathmap_logo,
			   NULL);
} /* dialog_about_callback */

/*****/

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
	case GTK_RESPONSE_DELETE_EVENT :
	    gtk_widget_destroy(widget);
	    break;

	case GTK_RESPONSE_HELP :
	    dialog_help_callback(0, 0);
	    break;

	default :
	    assert(0);
    }
}

/****/

static void
dialog_tree_changed (GtkTreeSelection *selection, gpointer data)
{
    GtkTreeModel * model;
    GtkTreeIter iter;

    if (selection == 0)
	return;

    if (gtk_tree_selection_get_selected(selection, &model, &iter))
    {
	GValue value = { 0, };
	const gchar *path;
	char *expression;

	gtk_tree_model_get_value(model, &iter, 1, &value);
	path = g_value_get_string(&value);
	if (path == 0)
	    return;

	expression = read_expression(path);
	if (expression == 0)
	{
	    char *message = g_strdup_printf(_("Could not read expression from file `%s'"), path);

	    gimp_message(message);
	    g_free(message);

	    return;
	}

	set_filter_source(expression, path);

	free(expression);
    }

    if (auto_preview)
	dialog_update_preview();
}

static void
designer_tree_callback (GtkTreeSelection *selection, gpointer data)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    if (selection == 0)
	return;

    if (gtk_tree_selection_get_selected(selection, &model, &iter))
    {
	GValue value = { 0, };
	const gchar *name;
	designer_design_t *design;
	designer_node_t *node;
	int i;

	gtk_tree_model_get_value(model, &iter, 2, &value);
	name = g_value_get_string(&value);
	if (name == NULL)
	    return;

	design = get_current_design();

	i = 1;
	node = NULL;
	do
	{
	    const char *numbered_name;

	    if (i == 1)
		numbered_name = name;
	    else
		numbered_name = g_strdup_printf("%s_%d", name, i);

	    if (designer_get_node_by_name(design, numbered_name) == NULL)
		node = designer_add_node(design, numbered_name, name);

	    if (numbered_name != name)
		g_free((gpointer)numbered_name);

	    ++i;
	} while (node == NULL);

	designer_widget_add_node(designer_widget, node, 10, 10);
	design_changed_callback(designer_widget, design);
    }
}
