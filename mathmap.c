/* The GIMP -- an image manipulation program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * MathMap plug-in --- generate an image by means of a mathematical expression
 * Copyright (C) 1997-2010 Mark Probst
 * mark.probst@gmail.com
 *
 * Copyright (C) 2008 Serge van Thillo
 * nulleke@hotmail.com
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
#include <sys/time.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#ifdef THREADED_FINAL_RENDER
#include <pthread.h>
#endif

#include <glib.h>
#include <glib/gstdio.h>
#include <gtk/gtk.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <gsl/gsl_errno.h>
#ifdef USE_GTKSOURCEVIEW
#include <gtksourceview/gtksourceview.h>
#include <gtksourceview/gtksourcelanguage.h>
#ifdef USE_GTKSOURCEVIEW1
#include <gtksourceview/gtksourcelanguagesmanager.h>
#else
#include <gtksourceview/gtksourcelanguagemanager.h>
#endif
#endif

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
#include "expression_db.h"
#include "designer/designer.h"

#define DEFAULT_PREVIEW_SIZE	384

/* Even more stuff from Quartics plugins */
#define CHECK_SIZE  8
#define CHECK_DARK  ((int) (1.0 / 3.0 * 255))
#define CHECK_LIGHT ((int) (2.0 / 3.0 * 255))

#ifdef __MINGW32__
#define SOURCEVIEW_FONT "Courier New 10"
#else
#define SOURCEVIEW_FONT "DejaVu Sans Mono Book 7"
#endif

#define EXPRESSIONS_DIR         "expressions"

#define DEFAULT_EXPRESSION \
"# Welcome to MathMap!\n" \
"\n" \
"# MathMap is a programmable tool for image manipulation.\n" \
"\n" \
"# Here in the \"Expression\" tab is where you can enter and\n" \
"# modify MathMap programs, called \"filters\".  To get you\n" \
"# started there is a very simple filter which doesn't modify\n" \
"# its input image down below.\n" \
"\n" \
"# In the \"Filters\" tab you'll find lots of more interesting\n" \
"# filters you can try out, study and modify.  Most of those\n" \
"# filters have parameters that you can play around with in\n" \
"# the \"User Values\" tab.\n" \
"\n" \
"# Finally, the \"Composer\" tab provides a graphical way of\n" \
"# combining two or more filters into a more complex\n" \
"# \"composition\", which in turn can be used in yet more\n" \
"# complex ones.\n" \
"\n" \
"\n" \
"# This is the simple demo filter.  It's called \"demo\".\n" \
"# \"image in\" means that the filter takes a single input,\n" \
"# namely an image that we call \"in\".\n" \
"filter demo (image in)\n" \
"  # A filter must calculate a pixel at a specific position\n" \
"  # which is called \"xy\", which is a coordinate pair consisting\n" \
"  # of the X-coordinate \"x\" and the Y-coordinate \"y\".\n" \
"\n" \
"  # Here we just get the pixel at the same position from the\n" \
"  # input image:\n" \
"  in(xy)\n" \
"\n" \
"  # Another. even simpler possibility, is to always produce\n" \
"  # the same color, for example:\n" \
"  #grayColor(0.5)\n" \
"\n" \
"  # Or we can produce one color if the X-coordinate is\n" \
"  # positive, and a different one otherwise:\n" \
"  #if x>0 then rgbColor(1,0,0) else rgbColor(0,1,0) end\n" \
"end\n"

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

static gboolean generate_code (void);

static void do_mathmap (int frame_num, float t);
static gint32 mathmap_layer_copy (gint32 layerID);

static void update_userval_table (void);

static void load_design (const char *filename);

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

static void dialog_load_callback (GtkWidget *widget, gpointer data);
static void dialog_save_callback (GtkWidget *widget, gpointer data);
static void dialog_save_as_callback (GtkWidget *widget, gpointer data);
static void dialog_ok_callback (GtkWidget *widget, gpointer data);
static void dialog_help_callback (GtkWidget *widget, gpointer data);
static void dialog_about_callback (GtkWidget *widget, gpointer data);
static void dialog_tree_changed (GtkTreeSelection *tree, gpointer data);
static void dialog_response (GtkWidget *widget, gint response_id, gpointer data);

static void designer_tree_callback (GtkTreeSelection *tree, gpointer data);
static void design_save_callback (GtkWidget *widget, gpointer data);
static void design_save_as_callback (GtkWidget *widget, gpointer data);

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

static gboolean ignore_dialog_tree_changes = FALSE;
static gboolean ignore_designer_tree_changes = FALSE;

//static int debug_tuples = 0;
//static pixel_debug_info_t pixel_debug_infos[PREVIEW_SIZE * PREVIEW_SIZE];

GtkTextBuffer *source_buffer;
GtkTextMark *source_marker = NULL;
GtkWidget *mathmap_dialog_window,
    *expression_entry,
    *animation_table,
    *frame_table,
    *edge_color_x_well,
    *edge_color_y_well,
    *uservalues_scrolled_window,
    *uservalues_table,
    *tree_scrolled_window,
    *designer_widget,
    *designer_tree_scrolled_window,
    *notebook;

#ifdef THREADED_FINAL_RENDER
pthread_mutex_t get_gimp_pixel_mutex;
#define NUM_FINAL_RENDER_CPUS		(get_num_cpus())
#else
#define NUM_FINAL_RENDER_CPUS		1
#endif

int previewing = 0, auto_preview = 1, fast_preview = 1;
int expression_changed = 1;
color_t gradient_samples[USER_GRADIENT_POINTS];
int output_bpp;
int edge_behaviour_x_mode = EDGE_BEHAVIOUR_COLOR;
int edge_behaviour_y_mode = EDGE_BEHAVIOUR_COLOR;

int fast_image_source_scale;

static GimpRGB edge_color_x = { 0.0, 0.0, 0.0, 0.0 };
static GimpRGB edge_color_y = { 0.0, 0.0, 0.0, 0.0 };

mathmap_t *mathmap = NULL;
mathmap_invocation_t *invocation = NULL;

static char *current_filename = NULL;
static char *current_design_filename = NULL;

static expression_db_t *filters_edb = NULL;
static expression_db_t *designer_edb = NULL;
static designer_design_type_t *the_design_type = NULL;
static designer_design_t *the_current_design = NULL;

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
set_current_design_filename (const char *new_filename)
{
    if (current_design_filename != NULL)
	g_free(current_design_filename);

    if (new_filename == NULL)
	current_design_filename = NULL;
    else
	current_design_filename = g_strdup(new_filename);
}

static void
set_filter_source (const char *source, const char *path)
{
    set_current_filename(path);

    delete_expression_marker();
    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(source_buffer), source, strlen(source));

    expression_copy(mmvals.expression, source);
}

/*****/

static gint
my_gimp_main (const GimpPlugInInfo *info, int argc, char *argv[])
{
    int i;

    gsl_set_error_handler_off();
    init_gettext();

    for (i = 0; i < USER_GRADIENT_POINTS; ++i)
    {
	float v = (float)i / (float)(USER_GRADIENT_POINTS - 1);

	gradient_samples[i] = MAKE_RGBA_COLOR_FLOAT(v, v, v, 1.0);
    }

    for (i = 1; i < argc; ++i)
	if (strcmp(argv[i], "-gimp") == 0)
	{
	    cmd_line_mode = 0;
	    return gimp_main(info, argc, argv);
	}

    cmd_line_mode = 1;
    return cmdline_main(argc, argv);
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
lookup_rc_file (char *name, gboolean report_error)
{
    gchar *local_filename, *global_filename, *filename;

    local_filename = get_rc_file_name(name, 0);

    if (g_file_test(local_filename, G_FILE_TEST_EXISTS))
	filename = local_filename;
    else
    {
	global_filename = get_rc_file_name(name, 1);

	if (g_file_test(global_filename, G_FILE_TEST_EXISTS))
	{
	    g_free(local_filename);
	    filename = global_filename;
	}
	else
	{
	    if (report_error)
		g_warning(_("Could not find file `%s' - should be either `%s' or `%s'."),
			  name, local_filename, global_filename);
	    g_free(local_filename);
	    g_free(global_filename);
	    filename = NULL;
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
gimp_plugin_menu_register (mathmap, "<Image>/Filters/Generic/");
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

    if (strncmp(name, "mathmap_", 8) == 0)
    {
	char *exp = expression_for_symbol(name + 8, read_expressions());

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
    drawable = alloc_gimp_input_drawable(gimp_drawable_get(param[2].data.d_drawable), TRUE);
    assert(drawable != 0);

    output_bpp = drawable->v.gimp.bpp;

    gimp_drawable = get_gimp_input_drawable(drawable);

    /* Init MathMap engine */
    init_builtins();
    init_tags();
    init_macros();
    init_compiler();

#ifdef THREADED_FINAL_RENDER
    pthread_mutex_init(&get_gimp_pixel_mutex, NULL);
#endif

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

		if (!generate_code())
		    status = GIMP_PDB_CALLING_ERROR;
	    }

	    break;

	case GIMP_RUN_WITH_LAST_VALS:
	    /* Possibly retrieve data */

	    gimp_get_data(name, &mmvals);

	    if (!generate_code())
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
		sprintf(layer_name, _("Frame %d"), frame + 1);
		gimp_drawable_set_name(layer, layer_name);
		output_drawable = gimp_drawable_get(layer);
		gimp_image_insert_layer(image_id, layer, 0, 0);
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

void
mathmap_message_dialog (const char *message)
{
    GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(mathmap_dialog_window),
						GTK_DIALOG_DESTROY_WITH_PARENT,
						GTK_MESSAGE_ERROR,
						GTK_BUTTONS_CLOSE,
						"%s", message);
    g_signal_connect_swapped (dialog, "response",
			      G_CALLBACK (gtk_widget_destroy),
			      dialog);
    gtk_window_set_title(GTK_WINDOW(dialog), _("MathMap Message"));
    gtk_widget_show(dialog);
}

void
mathmap_message_dialog_modal (const char *message)
{
    GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(mathmap_dialog_window),
						GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
						GTK_MESSAGE_ERROR,
						GTK_BUTTONS_CLOSE,
						"%s", message);
    gtk_window_set_title(GTK_WINDOW(dialog), _("MathMap Message"));
    gtk_widget_show(dialog);

    gtk_dialog_run(GTK_DIALOG(dialog));

    gtk_widget_destroy(dialog);
}

/*****/

static void
node_focussed_callback (GtkWidget *widget, designer_node_t *node)
{
    char *source;

    source = make_filter_source_from_design(node->design, "__composer_filter__");

    set_filter_source(source, NULL);

    g_free(source);
}

static void
design_changed_callback (GtkWidget *widget, designer_design_t *design)
{
    designer_node_t *node = design->root;

    if (node != NULL)
	node_focussed_callback(widget, node);
}

static gboolean
node_title_change_callback (GtkWidget *widget, designer_node_t *node, const char *name)
{
    if (strlen (name) == 0)
    {
	mathmap_message_dialog_modal(_("Node title cannot be empty"));
	return FALSE;
    }

    if (designer_node_set_name (node, name))
    {
	design_changed_callback(widget, node->design);
	return TRUE;
    }

    mathmap_message_dialog_modal(_("Another node already has that name"));

    return FALSE;
}

static void
load_design (const char *filename)
{
    designer_design_t *design;

    design = designer_load_design(the_design_type, filename,
				  &designer_widget_design_loaded_callback,
				  &designer_widget_node_aux_load_callback,
				  NULL,
				  designer_widget);

    if (design == NULL)
    {
	char *message = g_strdup_printf(_("Cannot read composer file `%s'"), filename);

	mathmap_message_dialog(message);
	g_free(message);

	return;
    }

    /* FIXME: free old design */

    the_current_design = design;
    set_current_design_filename(filename);

    design_changed_callback(designer_widget, the_current_design);
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

gradient_t*
get_default_gradient (void)
{
    static gradient_t gradient;
    static gboolean inited = FALSE;

    if (!inited)
    {
	gradient.values = gradient_samples;
	inited = TRUE;
    }

    return &gradient;
}

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

static gboolean
generate_code (void)
{
    if (expression_changed)
    {
	static char *support_paths[3];

	mathmap_t *new_mathmap;

	if (run_mode == GIMP_RUN_INTERACTIVE && expression_entry != 0)
	    dialog_text_update();

	if (mathmap != 0)
	    unload_mathmap(mathmap);

	if (!support_paths[0])
	{
	    support_paths[0] = get_rc_file_name(NULL, FALSE);
	    support_paths[1] = get_rc_file_name(NULL, TRUE);
	    support_paths[2] = NULL;
	}

	new_mathmap = compile_mathmap(mmvals.expression, support_paths, DEFAULT_OPTIMIZATION_TIMEOUT, FALSE);

	if (new_mathmap == 0)
	{
	    g_print(_("Error: %s\n"), error_string);
	    mathmap_message_dialog(error_string);

	    if (scanner_region_is_valid(error_region))
		set_expression_marker(error_region.start.row, error_region.start.column,
				      error_region.end.row, error_region.end.column);

	    /* FIXME: free old mathmap/invocation */

	    mathmap = 0;
	    invocation = 0;
	}
	else
	{
	    mathmap_invocation_t *new_invocation;

	    new_invocation = invoke_mathmap(new_mathmap, invocation, sel_width, sel_height, FALSE);
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
	invocation_set_antialiasing(invocation, mmvals.flags & FLAG_ANTIALIASING);
	invocation->supersampling = mmvals.flags & FLAG_SUPERSAMPLING;

	invocation->edge_behaviour_x = edge_behaviour_x_mode;
	invocation->edge_behaviour_y = edge_behaviour_y_mode;
	invocation->edge_color_x = MAKE_RGBA_COLOR_FLOAT(edge_color_x.r, edge_color_x.g, edge_color_x.b, edge_color_x.a);
	invocation->edge_color_y = MAKE_RGBA_COLOR_FLOAT(edge_color_y.r, edge_color_y.g, edge_color_y.b, edge_color_y.a);
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
alloc_gimp_input_drawable (GimpDrawable *gimp_drawable, gboolean honor_selection)
{
    int x, y, width, height;
    input_drawable_t *drawable;

    if (honor_selection)
    {
	gint x1, x2, y1, y2;

	gimp_drawable_mask_bounds(GIMP_DRAWABLE_ID(gimp_drawable), &x1, &y1, &x2, &y2);

	x = x1;
	y = y1;
	width = x2 - x1;
	height = y2 - y1;
    }
    else
    {
	x = y = 0;
	width = gimp_drawable_width(GIMP_DRAWABLE_ID(gimp_drawable));
	height = gimp_drawable_height(GIMP_DRAWABLE_ID(gimp_drawable));
    }

    drawable = alloc_input_drawable(INPUT_DRAWABLE_GIMP, width, height);

    drawable->v.gimp.drawable = gimp_drawable;
    drawable->v.gimp.has_selection = honor_selection;
    drawable->v.gimp.x0 = x;
    drawable->v.gimp.y0 = y;
    drawable->v.gimp.bpp = gimp_drawable_bpp(GIMP_DRAWABLE_ID(gimp_drawable));
    drawable->v.gimp.row = -1;
    drawable->v.gimp.col = -1;
    drawable->v.gimp.tile = 0;
    drawable->v.gimp.fast_image_source = 0;

    drawable->v.gimp.fast_image_source_width =
	(drawable->image.pixel_width + fast_image_source_scale - 1) / fast_image_source_scale;
    drawable->v.gimp.fast_image_source_height =
	(drawable->image.pixel_height + fast_image_source_scale - 1) / fast_image_source_scale;

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
    guint64 progress, max_progress;
    gchar progress_info[30];

    assert(invocation != 0);

    previewing = 0;

    if (generate_code())
    {
	mathmap_frame_t *frame;
	image_t *closure = closure_image_alloc(&invocation->mathfuncs, NULL,
					       invocation->mathmap->main_filter->num_uservals, invocation->uservals,
					       sel_width, sel_height);

	/* Initialize pixel region */
	gimp_pixel_rgn_init(&dest_rgn, output_drawable, sel_x1, sel_y1, sel_width, sel_height,
			    TRUE, TRUE);

	progress = 0;
	max_progress = ((guint64) sel_width) * ((guint64) sel_height);

	if (frame_num >= 0)
	    sprintf(progress_info, _("Mathmapping frame %d..."), frame_num + 1);
	else
	    strcpy(progress_info, _("Mathmapping..."));
	gimp_progress_init(progress_info);

	frame = invocation_new_frame(invocation, closure,
				     frame_num, current_t);

	for (pr = gimp_pixel_rgns_register(1, &dest_rgn);
	     pr != NULL; pr = gimp_pixel_rgns_process(pr))
	{
	    int region_x = dest_rgn.x - sel_x1;
	    int region_y = dest_rgn.y - sel_y1;
	    int region_width = dest_rgn.w;
	    int region_height = dest_rgn.h;

	    invocation->row_stride = dest_rgn.rowstride;
	    invocation->output_bpp = gimp_drawable_bpp(GIMP_DRAWABLE_ID(output_drawable));

	    call_invocation_parallel_and_join(frame, closure, region_x, region_y, region_width, region_height,
					      dest_rgn.data, NUM_FINAL_RENDER_CPUS);

	    /* Update progress */
	    progress += ((guint64) region_width) * ((guint64) region_height);
	    gimp_progress_update(((double) progress) / ((double)max_progress));
	}

	invocation_free_frame(frame);

	unref_tiles();

	gimp_drawable_flush(output_drawable);
	gimp_drawable_merge_shadow(GIMP_DRAWABLE_ID(output_drawable), TRUE);
	gimp_drawable_update(GIMP_DRAWABLE_ID(output_drawable), sel_x1, sel_y1, sel_width, sel_height);

	closure_image_free(closure);
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

    if (x < 0 || x >= drawable->image.pixel_width)
	return invocation->edge_color_x;
    if (y < 0 || y >= drawable->image.pixel_height)
	return invocation->edge_color_y;

    if (cmd_line_mode)
	return cmdline_mathmap_get_pixel(invocation, drawable, frame, x, y);

    g_assert(drawable->kind == INPUT_DRAWABLE_GIMP);

    x += drawable->v.gimp.x0;
    y += drawable->v.gimp.y0;

    newcol = x / tile_width;
    newcoloff = x % tile_width;
    newrow = y / tile_height;
    newrowoff = y % tile_height;

#ifdef THREADED_FINAL_RENDER
    pthread_mutex_lock(&get_gimp_pixel_mutex);
#endif

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

#ifdef THREADED_FINAL_RENDER
    pthread_mutex_unlock(&get_gimp_pixel_mutex);
#endif

    return MAKE_RGBA_COLOR(r, g, b, a);
}

static void
build_fast_image_source (input_drawable_t *drawable)
{
    int width, height;
    int x, y;
    int img_width, img_height;

    if (drawable->v.gimp.fast_image_source != 0)
	return;

    width = drawable->v.gimp.fast_image_source_width;
    height = drawable->v.gimp.fast_image_source_height;

    drawable->v.gimp.fast_image_source = g_malloc(width * height * sizeof(color_t));

    img_width = drawable->image.pixel_width;
    img_height = drawable->image.pixel_height;

    for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	    drawable->v.gimp.fast_image_source[x + y * width] =
		get_pixel(invocation, drawable, 0, x * img_width / width, y * img_height / height);
}

static color_t
get_pixel_fast (mathmap_invocation_t *invocation, input_drawable_t *drawable, int x, int y)
{
    x = x / fast_image_source_scale;
    y = y / fast_image_source_scale;

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

#define TREE_VALUE_NAME			0
#define TREE_VALUE_EDB			1

static void
tree_from_expression_db (GtkTreeStore *store, GtkTreeIter *parent, expression_db_t *edb)
{
    for (; edb != 0; edb = edb->next)
    {
	GtkTreeIter iter;

	if (edb->kind == EXPRESSION_DB_GROUP)
	{
	    gtk_tree_store_append(store, &iter, parent);
	    gtk_tree_store_set(store, &iter,
			       TREE_VALUE_NAME, edb->name,
			       -1);

	    tree_from_expression_db(store, &iter, edb->v.group.subs);
	}
	else if (edb->kind == EXPRESSION_DB_EXPRESSION || edb->kind == EXPRESSION_DB_DESIGN)
	{
	    gtk_tree_store_append(store, &iter, parent);
	    gtk_tree_store_set(store, &iter,
			       TREE_VALUE_NAME, edb->name,
			       TREE_VALUE_EDB, edb,
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
    store = gtk_tree_store_new(2, G_TYPE_STRING, G_TYPE_POINTER);

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
    column = gtk_tree_view_column_new_with_attributes(_("Filters"), renderer,
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
    if (run_mode != GIMP_RUN_INTERACTIVE)
	return;

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

typedef struct
{
    designer_node_t *node;
    double x;
    double y;
} node_and_position_t;

static void
update_expression_tree (void)
{
    designer_design_type_t *new_design_type;
    designer_design_t *new_design = NULL;
    int num_nodes = -1;
    node_and_position_t *positions = NULL;

    if (filters_edb != NULL)
	free_expression_db(filters_edb);
    if (designer_edb != NULL)
	free_expression_db(designer_edb);

    filters_edb = read_expressions();

    designer_edb = copy_expression_db(filters_edb);
    new_design_type = design_type_from_expression_db(&designer_edb);

    update_expression_tree_from_edb(tree_scrolled_window, filters_edb, G_CALLBACK(dialog_tree_changed));
    update_expression_tree_from_edb(designer_tree_scrolled_window, designer_edb, G_CALLBACK(designer_tree_callback));

    if (the_current_design != NULL)
    {
	GSList *list;
	int i;

	new_design = designer_migrate_design(the_current_design, new_design_type);
	g_assert(new_design != NULL);

	num_nodes = g_slist_length(new_design->nodes);
	positions = g_new(node_and_position_t, num_nodes);

	for (i = 0, list = new_design->nodes;
	     list != NULL;
	     ++i, list = list->next)
	{
	    designer_node_t *old_node;

	    positions[i].node = list->data;
	    old_node = designer_get_node_by_name(the_current_design, positions[i].node->name);
	    g_assert(old_node != NULL);

	    designer_widget_get_node_position(designer_widget, old_node, &positions[i].x, &positions[i].y);
	}
    }

    if (the_current_design != NULL)
    {
	designer_free_design(the_current_design);
	the_current_design = NULL;
    }
    if (the_design_type != NULL)
    {
	designer_free_design_type(the_design_type);
	the_design_type = NULL;
    }

    the_design_type = new_design_type;
    the_current_design = new_design;

    if (the_current_design == NULL)
    {
	the_current_design = designer_make_design(the_design_type, "__untitled_composition__");
	g_assert(the_current_design != NULL);
    }

    g_assert(designer_widget != NULL);
    designer_widget_set_design(designer_widget, the_current_design);

    if (positions != NULL)
    {
	int i;

	for (i = 0; i < num_nodes; ++i)
	    designer_widget_move_node(designer_widget, positions[i].node, positions[i].x, positions[i].y);

	g_free(positions);
    }

    ignore_dialog_tree_changes = TRUE;
    ignore_designer_tree_changes = TRUE;
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

static GtkWidget*
make_load_save_table (GtkWidget *content, GtkSignalFunc load_callback, GtkSignalFunc save_callback, GtkSignalFunc save_as_callback)
{
    GtkWidget *table, *button;
    int x = 0;

    table = gtk_table_new(load_callback ? 3 : 2, 2, FALSE);
    gtk_container_border_width(GTK_CONTAINER(table), 0);
    gtk_table_set_col_spacings(GTK_TABLE(table), 4);
    gtk_widget_show(table);

    gtk_table_attach(GTK_TABLE(table), content, 0, load_callback ? 3 : 2, 0, 1,
		     GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND, 0, 0);

    if (load_callback)
    {
	button = gtk_button_new_with_label(_("Load..."));
	gtk_signal_connect(GTK_OBJECT(button), "clicked", load_callback, 0);
	gtk_table_attach(GTK_TABLE(table), button, x, x + 1, 1, 2, GTK_FILL, 0, 0, 0);
	gtk_widget_show(button);
	++x;
    }

    button = gtk_button_new_with_label(_("Save"));
    gtk_signal_connect(GTK_OBJECT(button), "clicked", save_callback, 0);
    gtk_table_attach(GTK_TABLE(table), button, x, x + 1, 1, 2, GTK_FILL, 0, 0, 0);
    gtk_widget_show(button);
    ++x;

    button = gtk_button_new_with_label(_("Save As..."));
    gtk_signal_connect(GTK_OBJECT(button), "clicked", save_as_callback, 0);
    gtk_table_attach(GTK_TABLE(table), button, x, x + 1, 1, 2, GTK_FILL, 0, 0, 0);
    gtk_widget_show(button);

    return table;
}

static GdkPixbuf*
load_pixbuf (char *name)
{
    char *filename = lookup_rc_file(name, TRUE);
    GdkPixbuf *pixbuf;

    if (!filename)
	return NULL;

    pixbuf = gdk_pixbuf_new_from_file(filename, NULL);
    if (!pixbuf)
	g_warning("Could not load image file `%s'.", filename);
    g_free(filename);

    return pixbuf;
}

#if defined(USE_GTKSOURCEVIEW) && !defined(USE_GTKSOURCEVIEW1)
static GtkSourceLanguageManager*
make_source_language_manager (void)
{
    GtkSourceLanguageManager *manager = gtk_source_language_manager_new();
    const gchar* const* paths = gtk_source_language_manager_get_search_path(manager);
    gchar **new_paths;
    int count, i;

    for (count = 0; paths[count] != NULL; ++count)
	;

    new_paths = g_new(gchar*, count + 3);

    for (i = 0; i < count; ++i)
	new_paths[i] = (gchar*)paths[i];

    new_paths[count + 0] = get_rc_file_name(NULL, FALSE);
    new_paths[count + 1] = get_rc_file_name(NULL, TRUE);
    new_paths[count + 2] = NULL;

    gtk_source_language_manager_set_search_path(manager, new_paths);

    g_free(new_paths[count + 0]);
    g_free(new_paths[count + 1]);
    g_free(new_paths);

    return manager;
}
#endif

#define NOTEBOOK_PAGE_EXPRESSION	0
#define NOTEBOOK_PAGE_SETTINGS		1
#define NOTEBOOK_PAGE_USERVALS		2
#define NOTEBOOK_PAGE_FILTERS		3
#define NOTEBOOK_PAGE_DESIGNER		4

static gint
mathmap_dialog (int mutable_expression)
{
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
    GtkWidget *t_table;
    GtkWidget *spin_button;
    GtkObject *adjustment;
    GdkPixbuf *pixbuf;

    gimp_ui_init("mathmap", TRUE);

    alloc_preview_pixbuf(DEFAULT_PREVIEW_SIZE, DEFAULT_PREVIEW_SIZE);

    mathmap_dialog_window = gimp_dialog_new("MathMap", "mathmap",
					    NULL, 0,
					    gimp_standard_help_func, "plug-in-mathmap",
					    GTK_STOCK_HELP, GTK_RESPONSE_HELP,
					    _("About"), RESPONSE_ABOUT,
					    GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					    GTK_STOCK_OK, GTK_RESPONSE_OK,
					    NULL);

    g_signal_connect (mathmap_dialog_window, "response",
		      G_CALLBACK (dialog_response),
		      NULL);

    g_signal_connect (mathmap_dialog_window, "destroy",
		      G_CALLBACK (gtk_main_quit),
		      NULL);

    top_table = gtk_hpaned_new();
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mathmap_dialog_window)->vbox), top_table, TRUE, TRUE, 0);
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
#ifdef USE_GTKSOURCEVIEW
#ifdef USE_GTKSOURCEVIEW1
	    GtkSourceLanguagesManager *manager;
#else
	    GtkSourceLanguageManager *manager;
#endif
	    GtkSourceLanguage *language;

	    /* Language */
#ifdef USE_GTKSOURCEVIEW1
	    manager = gtk_source_languages_manager_new();
	    language = gtk_source_languages_manager_get_language_from_mime_type(manager, "application/x-mathmap");
#else
	    manager = make_source_language_manager();
	    language = gtk_source_language_manager_get_language(manager, "mathmap");
#endif

	    /* Source Buffer */
	    source_buffer = GTK_TEXT_BUFFER(gtk_source_buffer_new(NULL));
	    if (language != NULL)
		gtk_source_buffer_set_language(GTK_SOURCE_BUFFER(source_buffer), language);
#ifdef USE_GTKSOURCEVIEW1
	    gtk_source_buffer_set_highlight(GTK_SOURCE_BUFFER(source_buffer), TRUE);
#else
	    gtk_source_buffer_set_highlight_syntax(GTK_SOURCE_BUFFER(source_buffer), TRUE);
#endif
#else
	    /* Source Buffer */
	    source_buffer = gtk_text_buffer_new(NULL);
#endif

	    /* Scrolled Window */
	    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
					   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	    gtk_widget_show(scrolled_window);

	    /* Source View */
#ifdef USE_GTKSOURCEVIEW
	    expression_entry = gtk_source_view_new_with_buffer(GTK_SOURCE_BUFFER(source_buffer));
#else
	    expression_entry = gtk_text_view_new_with_buffer(source_buffer);
#endif
	    gtk_widget_show(expression_entry);

#ifdef GTK_SOURCE_MARK
	    gtk_source_view_set_show_line_marks(GTK_SOURCE_VIEW(expression_entry), TRUE);

	    if ((pixbuf = load_pixbuf("error.png")))
	    {
		gtk_source_view_set_mark_category_pixbuf (GTK_SOURCE_VIEW (expression_entry), "one", pixbuf);
		g_object_unref (pixbuf);
	    }
#endif

	    gtk_container_add(GTK_CONTAINER(scrolled_window), expression_entry);

	    g_signal_connect(G_OBJECT(source_buffer), "changed",
			     G_CALLBACK(dialog_text_changed),
			     (gpointer)NULL);

	    gtk_text_buffer_set_text(source_buffer, mmvals.expression,
			    	     strlen(mmvals.expression));

	    font_desc = pango_font_description_from_string(SOURCEVIEW_FONT);
	    if (font_desc != NULL)
	    {
		gtk_widget_modify_font(expression_entry, font_desc);
		pango_font_description_free(font_desc);
	    }

	    table = make_load_save_table(scrolled_window,
					 (GtkSignalFunc)dialog_load_callback,
					 (GtkSignalFunc)dialog_save_callback,
					 (GtkSignalFunc)dialog_save_as_callback);

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
		adjustment = gtk_adjustment_new(mmvals.frames, 2, 9999, 1.0, 10.0, 0.0);
		spin_button = gtk_spin_button_new(GTK_ADJUSTMENT(adjustment), 10.0, 0);
		gtk_table_attach (GTK_TABLE (frame_table), spin_button, 1, 2, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_signal_connect (GTK_OBJECT (adjustment), "value_changed",
				    (GtkSignalFunc) dialog_scale_update,
				    &mmvals.frames);
		gtk_widget_show(label);
		gtk_widget_show(spin_button);

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

	if (mutable_expression)
	{

	/* Examples */

	    tree_scrolled_window = make_tree_scrolled_window();

	    label = gtk_label_new(_("Filters"));
	    gtk_widget_show(label);
	    gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), tree_scrolled_window, label, label);

	/* Designer */

	    hpaned = gtk_hpaned_new();

	    designer_tree_scrolled_window = make_tree_scrolled_window();
	    gtk_widget_set_size_request(designer_tree_scrolled_window, 150, 200);
	    gtk_paned_add1(GTK_PANED(hpaned), designer_tree_scrolled_window);

	    designer_widget = designer_widget_new(NULL,
						  design_changed_callback,
						  node_focussed_callback,
						  node_title_change_callback);
	    gtk_widget_set_size_request(designer_widget, 400, 400);
	    gtk_paned_add2(GTK_PANED(hpaned), designer_widget);

	    gtk_widget_show(hpaned);

	    table = make_load_save_table(hpaned,
					 NULL,
					 (GtkSignalFunc)design_save_callback,
					 (GtkSignalFunc)design_save_as_callback);

	    label = gtk_label_new(_("Composer"));
	    gtk_widget_show(label);
	    gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), table, label, label);

	    update_expression_tree();
	}

    /* Done */

    if (!mutable_expression)
	dialog_update_preview();

    gtk_widget_show(mathmap_dialog_window);

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

#ifdef PRINT_FPS
static void
calculate_and_print_fps (void)
{
    static gboolean have_last_tv = FALSE;
    static struct timeval last_tv;

    struct timeval tv;
    int micros;

    if (!have_last_tv)
    {
	gettimeofday(&last_tv, NULL);
	have_last_tv = TRUE;
	return;
    }

    gettimeofday(&tv, NULL);

    micros = (tv.tv_sec - last_tv.tv_sec) * 1000000 + (tv.tv_usec - last_tv.tv_usec);

    g_printf("%f fps\n", 1000000.0 / micros);

    last_tv = tv;
}
#endif

static gboolean
recalculate_preview (void)
{
    static int in_recalculate = 0;

#ifdef PRINT_FPS
    calculate_and_print_fps();
#endif

    if (in_recalculate > 0)
	return FALSE;

    ++in_recalculate;

    if (generate_code())
    {
	int preview_width = gdk_pixbuf_get_width(wint.pixbuf);
	int preview_height = gdk_pixbuf_get_height(wint.pixbuf);
	int x, y;
	guchar *p_ul, *p;
	gint check, check_0, check_1;
	guchar *buf = (guchar*)malloc(4 * preview_width * preview_height);
	int old_render_width, old_render_height;
	mathmap_frame_t *frame;
	image_t *closure = closure_image_alloc(&invocation->mathfuncs, NULL,
					       invocation->mathmap->main_filter->num_uservals, invocation->uservals,
					       preview_width, preview_height);
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

	old_render_width = invocation->render_width;
	old_render_height = invocation->render_height;

	if (previewing)
	{
	    invocation->render_width = preview_width;
	    invocation->render_height = preview_height;
	}
	else
	{
	    invocation->render_width = invocation->img_width;
	    invocation->render_height = invocation->img_height;
	}

	if (previewing)
	    for_each_input_drawable(build_fast_image_source);

	frame = invocation_new_frame(invocation, closure, 0, mmvals.param_t);

	frame->frame_render_width = preview_width;
	frame->frame_render_height = preview_height;

	if (previewing)
	    call_invocation_parallel_and_join(frame, closure, 0, 0, preview_width, preview_height,
					      buf, get_num_cpus());
	else
	    call_invocation_parallel_and_join(frame, closure, 0, 0, preview_width, preview_height,
					      buf, NUM_FINAL_RENDER_CPUS);

	invocation_free_frame(frame);

	invocation->render_width = old_render_width;
	invocation->render_height = old_render_height;

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
	closure_image_free(closure);

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
    if (source_marker != NULL)
    {
        gtk_text_buffer_delete_mark(source_buffer, source_marker);
	source_marker = 0;
    }
}

void
set_expression_marker (int start_line, int start_column, int end_line, int end_column)
{
    delete_expression_marker();

    if (expression_entry != 0)
    {
	GtkTextIter start_iter, end_iter;

#ifdef DEBUG_OUTPUT
	g_print("Error at %d:%d - %d:%d\n", start_line, start_column, end_line, end_column);
#endif

	if (start_line == end_line && start_column == end_column)
	    ++end_column;

	gtk_text_buffer_get_iter_at_line_index(source_buffer, &start_iter, start_line, start_column);
	gtk_text_buffer_get_iter_at_line_index(source_buffer, &end_iter, end_line, end_column);
#ifdef GTK_SOURCE_MARK
	source_marker = GTK_TEXT_MARK(gtk_source_buffer_create_source_mark(GTK_SOURCE_BUFFER(source_buffer), NULL, "one", &start_iter));
#endif
	gtk_text_buffer_select_range(source_buffer, &start_iter, &end_iter);
	gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(expression_entry),
					   gtk_text_buffer_get_insert(source_buffer));
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
    if (gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)) == NOTEBOOK_PAGE_DESIGNER)
	design_changed_callback(designer_widget, the_current_design);

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

	mathmap_message_dialog(message);
	g_free(message);

	return;
    }

    len = strlen(mmvals.expression);
    if (fwrite(mmvals.expression, 1, len, file) != len)
    {
	char *message = g_strdup_printf(_("Could not write to file `%s': %m"), current_filename);

	mathmap_message_dialog(message);
	g_free(message);
    }

    fclose(file);
}

static char*
load_save_dialog (gboolean is_load, const char *title, const char *filename, const char *default_filename)
{
    GtkWidget *dialog;
    char *result;

    dialog = gtk_file_chooser_dialog_new (title,
					  NULL,
					  is_load ? GTK_FILE_CHOOSER_ACTION_OPEN : GTK_FILE_CHOOSER_ACTION_SAVE,
					  GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					  is_load ? GTK_STOCK_OPEN : GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
					  NULL);
    if (!is_load)
	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);

    if (filename == NULL)
    {
	char *mathmap_path = get_rc_file_name(0, 0);
	char *default_path = get_rc_file_name(EXPRESSIONS_DIR, 0);

	/* We try to create the directory just in case.  If it already
	   exists, nothing happens and it doesn't hurt. */
	g_mkdir(mathmap_path, 0777);
	g_mkdir(default_path, 0777);

	gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), default_path);
	gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog), default_filename);

	g_free(mathmap_path);
	g_free(default_path);
    }
    else
	gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (dialog), filename);

    if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
	result = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    else
	result = NULL;

    gtk_widget_destroy (dialog);

    return result;
}

static void
dialog_load_callback (GtkWidget *widget, gpointer data)
{
    char *filename = load_save_dialog(TRUE, _("Load Expression"), current_filename, NULL);
    char *contents;
    gsize length;

    if (filename != NULL)
    {
	if (g_file_get_contents (filename, &contents, &length, NULL))
	{
	    set_filter_source(contents, filename);
	    g_free(contents);
	}
	g_free(filename);
    }
}

static void
dialog_save_as_callback (GtkWidget *widget, gpointer data)
{
    char *filename = load_save_dialog(FALSE, _("Save Expression"), current_filename, _("Untitled expression.mm"));

    if (filename != NULL)
    {
	set_current_filename(filename);
	g_free(filename);

	save_expression();

	update_expression_tree();
    }
}

static void
dialog_save_callback (GtkWidget *widget, gpointer data)
{
    if (current_filename == 0)
	dialog_save_as_callback(widget, data);
    else
    {
	save_expression();

	update_expression_tree();
    }
}

/*****/

static void
save_design (void)
{
    char *design_name, *p, *q;
    char *basename;

    g_assert(the_current_design != NULL);
    g_assert(current_design_filename != NULL);

    basename = g_path_get_basename(current_design_filename);
    design_name = g_malloc(strlen(basename) + 1);
    p = design_name;
    q = basename;
    while (*q != '\0')
    {
	if (g_ascii_isalpha(*q))
	    *p = g_ascii_tolower(*q);
	else if (g_ascii_isdigit(*q) || *q == '_')
	    *p = *q;
	else if (*q == '.')
	    break;
	else
	    *p = '_';
	++p;
	++q;
    }
    *p = '\0';

    designer_set_design_name(the_current_design, design_name);
    g_free(design_name);
    g_free(basename);

    designer_save_design(the_current_design, current_design_filename,
			 &designer_widget_node_aux_print,
			 NULL,
			 designer_widget);
}

static void
design_save_as_callback (GtkWidget *widget, gpointer data)
{
    char *filename = load_save_dialog(FALSE, _("Save Composition"), current_design_filename, _("Untitled composition.mmc"));

    if (filename != NULL)
    {
	set_current_design_filename(filename);
	g_free(filename);

	save_design();

	update_expression_tree();
    }
}

static void
design_save_callback (GtkWidget *widget, gpointer data)
{
    if (current_design_filename == NULL)
	design_save_as_callback(widget, data);
    else
    {
	save_design();

	update_expression_tree();
    }
}

/*****/

static void
dialog_ok_callback (GtkWidget *widget, gpointer data)
{
    if (generate_code())
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

    if (gimp_procedural_db_proc_info("plug-in-web-browser",
				     &proc_blurb, &proc_help, 
				     &proc_author, &proc_copyright, &proc_date,
				     &proc_type, &nparams, &nreturn_vals,
				     &params, &return_vals))
	gimp_run_procedure("plug-in-web-browser", &baz,
			   GIMP_PDB_STRING, MATHMAP_MANUAL_URL,
			   GIMP_PDB_END);
    else 
    {
	gchar *message = g_strdup_printf(_("See %s"), MATHMAP_MANUAL_URL);

	mathmap_message_dialog(message);
	g_free(message);
    }
} /* dialog_help_callback */

/*****/

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
    char *authors[] = { "Mark Probst <mark.probst@gmail.com>",
			"Herbert Poetzl <herbert@13thfloor.at>",
			"Genadz Batsyan <gbatyan@gmail.com>",
			"Simone Demmel <neko@greenie.muc.de>",
			"Carlos A. Furuti <carlos.furuti@progonos.com>",
			"Alexander Heide <heide@ra.physik.uni-halle.de>",
			"Eric Kidd <eric.kidd@pobox.com>",
			"Yuval Levy <yuval@levy.ch>",
			"Hans Lundmark <h.lundmark@gmail.com>",
			"Xavier Martin <xavier.martin@avedya.com>",
			"Tom Rathborne <tom.rathborne@gmail.com>",
			"Ben Reichardt <ben.reichardt@gmail.com>",
			"Josh Sommers <josh@sommers.net>",
			"Serge van Thillo <nulleke@hotmail.com>",
			"Andy Thomas",
			NULL };
    char *artists[] = { "Herbert Poetzl <herbert@13thfloor.at>", NULL };
    char *translators = "Laurent Despeyroux <not@fgrev.no>\nYury Aliaev <mutabor@altlinux.org>";

    if (mathmap_logo == NULL)
	mathmap_logo = load_pixbuf("mathmap.png");

    gtk_show_about_dialog (NULL,
			   "name", "MathMap",
			   "program-name", "MathMap",
			   "title", _("About MathMap"),
			   "version", MATHMAP_VERSION,
			   "authors", authors,
			   "artists", artists,
			   "translator-credits", translators,
			   "comments", _("An image generation and manipulation system"),
			   "website", "http://www.complang.tuwien.ac.at/schani/mathmap/",
			   "copyright", "Copyright © 1997-2009 Mark Probst, Herbert Poetzl, Genadz Batsyan",
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
    GtkTreeModel *model;
    GtkTreeIter iter;

    if (ignore_dialog_tree_changes)
    {
	ignore_dialog_tree_changes = FALSE;
	return;
    }

    if (selection == 0)
	return;

    if (gtk_tree_selection_get_selected(selection, &model, &iter))
    {
	GValue value = { 0, };
	expression_db_t *edb;

	gtk_tree_model_get_value(model, &iter, TREE_VALUE_EDB, &value);
	edb = g_value_get_pointer(&value);
	if (edb == NULL)
	    return;

	if (edb->kind == EXPRESSION_DB_EXPRESSION)
	{
	    char *path = edb->v.expression.path;
	    char *expression = read_expression(path);

	    if (expression == NULL)
	    {
		char *message = g_strdup_printf(_("Could not read expression from file `%s'"), path);

		mathmap_message_dialog(message);
		g_free(message);

		return;
	    }

	    set_filter_source(expression, path);

	    set_current_design_filename(NULL);

	    free(expression);
	}
	else if (edb->kind == EXPRESSION_DB_DESIGN)
	{
	    char *path = edb->v.design.path;

	    load_design(path);

	    set_current_filename(NULL);
	}
	else
	    g_assert_not_reached();
    }

    if (auto_preview)
	dialog_update_preview();
}

static void
designer_tree_callback (GtkTreeSelection *selection, gpointer data)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    if (ignore_designer_tree_changes)
    {
	ignore_designer_tree_changes = FALSE;
	return;
    }

    if (selection == 0)
	return;

    if (gtk_tree_selection_get_selected(selection, &model, &iter))
    {
	GValue value = { 0, };
	const gchar *name;
	designer_design_t *design = the_current_design;
	designer_node_t *node;
	designer_node_type_t *type;
	int i;
	expression_db_t *edb;

	gtk_tree_model_get_value(model, &iter, TREE_VALUE_EDB, &value);
	edb = g_value_get_pointer(&value);
	if (edb == NULL)
	    return;

	name = get_expression_name(edb, the_design_type);
	if (name == NULL)
	    return;

	type = designer_get_node_type_by_name(design->type, name);
	g_assert(type != NULL);

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
