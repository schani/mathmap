/* The GIMP -- an image manipulation program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * MathMap plug-in --- generate an image by means of a mathematical expression
 * Copyright (C) 1997-2000 Mark Probst
 * schani@unix.cslab.tuwien.ac.at
 *
 * Plug-In structure based on:
 *   Whirl plug-in --- distort an image into a whirlpool
 *   Copyright (C) 1997 Federico Mena Quintero
 *   federico@nuclecu.unam.mx
 *
 * Version 0.11
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

#define MATHMAP_VERSION       "0.11"

#define MATHMAP_MANUAL_URL    "http://www.complang.tuwien.ac.at/~schani/mathmap/manual.html"

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
#include "colorwell.h"
#include "noise.h"
#ifdef USE_CGEN
#include "cgen.h"
#endif

#ifdef USE_CGEN
#define EVAL_EXPR     eval_c_code
#else
#define EVAL_EXPR     eval_postfix
#endif

/***** Macros *****/

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))


/***** Magic numbers *****/

#define PREVIEW_SIZE 128
#define SCALE_WIDTH  200
#define ENTRY_WIDTH  60

/* Even more stuff from Quartics plugins */
#define CHECK_SIZE  8
#define CHECK_DARK  ((int) (1.0 / 3.0 * 255))
#define CHECK_LIGHT ((int) (2.0 / 3.0 * 255))   


#define DEFAULT_EXPRESSION      "origValXY(x+sin(y*10)*3,y+sin(x*10)*3)"
#define DEFAULT_NUMBER_FRAMES   10

#define FLAG_INTERSAMPLING      1
#define FLAG_OVERSAMPLING       2
#define FLAG_ANIMATION          4

#define MAX_EXPRESSION_LENGTH   8192

#define NUM_GRADIENT_SAMPLES    1024

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
    GDrawable *drawable;
    gint bpp;
    gint row;
    gint col;
    GTile *tile;
    guchar *fast_image_source;
    int used;
} input_drawable_t;

/***** Prototypes *****/

static void query(void);
static void run(char    *name,
		int      nparams,
		GParam  *param,
		int     *nreturn_vals,
		GParam **return_vals);

static void init_internals (void);

static void expression_copy (gchar *dest, gchar *src);

static void mathmap (int frame_num);
static gint32 mathmap_layer_copy(gint32 layerID);

extern int mmparse (void);

static void build_fast_image_source (input_drawable_t *drawable);

static void update_userval_table (void);

static void update_gradient (void);
static gint mathmap_dialog (int);
static void dialog_scale_update (GtkAdjustment *adjustment, gint *value);
static void dialog_t_update (GtkAdjustment *adjustment, gfloat *value);
static void dialog_text_changed (void);
static void dialog_text_update (void);
static void dialog_intersampling_update (GtkWidget *widget, gpointer data);
static void dialog_oversampling_update (GtkWidget *widget, gpointer data);
static void dialog_auto_preview_update (GtkWidget *widget, gpointer data);
static void dialog_fast_preview_update (GtkWidget *widget, gpointer data);
static void dialog_edge_behaviour_update (GtkWidget *widget, gpointer data);
static void dialog_edge_color_changed (ColorWell *color_well, gpointer data);
static void dialog_edge_color_set (void);
static void dialog_animation_update (GtkWidget *widget, gpointer data);
static void dialog_preview_callback (GtkWidget *widget, gpointer data);
static void dialog_close_callback (GtkWidget *widget, gpointer data);
static void dialog_ok_callback (GtkWidget *widget, gpointer data);
static void dialog_cancel_callback (GtkWidget *widget, gpointer data);
static void dialog_help_callback (GtkWidget *widget, gpointer data);
static void dialog_about_callback (GtkWidget *widget, gpointer data);
static void dialog_tree_changed (GtkTree *tree);

/***** Variables *****/

GPlugInInfo PLUG_IN_INFO = {
	NULL,   /* init_proc */
	NULL,   /* quit_proc */
	query,  /* query_proc */
	run     /* run_proc */
}; /* PLUG_IN_INFO */


static mathmap_vals_t mmvals = {
	FLAG_INTERSAMPLING,      /* flags */
	DEFAULT_NUMBER_FRAMES,   /* frames */
	0.0,                     /* t */
	DEFAULT_EXPRESSION       /* expression */
}; /* mmvals */

static mathmap_interface_t wint = {
	NULL,  /* preview */
	NULL,  /* wimage */
	FALSE  /* run */
}; /* wint */

#define MAX_INPUT_DRAWABLES 64

static GRunModeType run_mode;
static gint32 image_id;
static gint32 layer_id;
static input_drawable_t input_drawables[MAX_INPUT_DRAWABLES];
static GDrawable *output_drawable;

static gint   tile_width, tile_height;
gint   sel_x1, sel_y1, sel_x2, sel_y2;
gint   sel_width, sel_height;
gint   preview_width, preview_height;

static int imageWidth, imageHeight;

static double cen_x, cen_y;
static double scale_x, scale_y;
static double radius, radius2;

char error_string[1024];

GtkWidget *expression_entry = 0,
    *frame_table,
    *t_table,
    *edge_color_well,
    *uservalues_scrolled_window,
    *uservalues_table;
GtkColorSelectionDialog *color_selection_dialog;

exprtree *theExprtree = 0;
int img_width, img_height,
    originX,
    originY,
    outputBPP,
    previewing = 0,
    auto_preview = 1,
    fast_preview = 1;
int usesRA = 0,
    expression_changed = 1;
double currentX,
    currentY,
    currentR,
    currentA,
    currentT,
    imageR,
    imageX,
    imageY,
    imageW,
    imageH,
    middleX,
    middleY;
int intersamplingEnabled,
    oversamplingEnabled,
    animationEnabled = 1;
int edge_behaviour_color = 1,
    edge_behaviour_wrap = 2,
    edge_behaviour_reflect = 3;
int edge_behaviour_mode = 1;
unsigned char edge_color[4] = { 0, 0, 0, 0 };
int num_gradient_samples = NUM_GRADIENT_SAMPLES;
tuple_t gradient_samples[NUM_GRADIENT_SAMPLES];

/***** Functions *****/

/*****/

static void
expression_copy (gchar *dest, gchar *src)
{
    strncpy(dest, src, MAX_EXPRESSION_LENGTH);
    dest[MAX_EXPRESSION_LENGTH - 1] = 0;
}


/*****/

MAIN()


/*****/

static lisp_object_t*
read_rc_file (void)
{
    static lisp_object_t *obj = 0;

    gchar *filename;
    FILE *file;
    lisp_stream_t stream;

    if (obj != 0)
	return obj;

    filename = gimp_personal_rc_file("mathmaprc");
    file = fopen(filename, "r");
    g_free(filename);
    if (file == 0)
    {
	filename = g_strconcat(gimp_data_directory(), G_DIR_SEPARATOR_S, "mathmaprc", NULL);
	file = fopen(filename, "r");
	g_free(filename);

	if (file == 0)
	    return 0;
    }

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
	    static GParamDef args[] = {
		{ PARAM_INT32,      "run_mode",         "Interactive, non-interactive" },
		{ PARAM_IMAGE,      "image",            "Input image" },
		{ PARAM_DRAWABLE,   "drawable",         "Input drawable" },
		{ PARAM_INT32,      "flags",            "1: Intersampling 2: Oversampling 4: Animate" },
		{ PARAM_INT32,      "frames",           "Number of frames" },
		{ PARAM_FLOAT,      "param_t",          "The parameter t (if not animating)" }
	    };
	    static GParamDef *return_vals  = NULL;
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
				   "April 2000, " MATHMAP_VERSION,
				   menu,
				   "RGB*, GRAY*",
				   PROC_PLUG_IN,
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
expression_for_symbol (char *symbol, lisp_object_t *obj)
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
    static GParamDef args[] = {
	{ PARAM_INT32,      "run_mode",         "Interactive, non-interactive" },
	{ PARAM_IMAGE,      "image",            "Input image" },
	{ PARAM_DRAWABLE,   "drawable",         "Input drawable" },
	{ PARAM_INT32,      "flags",            "1: Intersampling 2: Oversampling 4: Animate" },
	{ PARAM_INT32,      "frames",           "Number of frames" },
	{ PARAM_FLOAT,      "param_t",          "The parameter t (if not animating)" },
	{ PARAM_STRING,     "expression",       "MathMap expression" }
    };
    static GParamDef *return_vals  = NULL;
    static int nargs = sizeof(args) / sizeof(args[0]);
    static int nreturn_vals = 0;

    gimp_install_procedure("plug_in_mathmap",
			   "Generate an image using a mathematical expression.",
			   "Generates an image by means of a mathematical expression. The expression "
			   "can also refer to the data of an original image. Thus, arbitrary "
			   "distortions can be constructed. Even animations can be generated.",
			   "Mark Probst",
			   "Mark Probst",
			   "April 2000, " MATHMAP_VERSION,
			   "<Image>/Filters/Generic/MathMap/MathMap",
			   "RGB*, GRAY*",
			   PROC_PLUG_IN,
			   nargs,
			   nreturn_vals,
			   args,
			   return_vals);

    register_examples();
}

/*****/

static void
run (char *name, int nparams, GParam *param, int *nreturn_vals, GParam **return_vals)
{
    static GParam values[1];

    GStatusType status;
    double xhsiz, yhsiz;
    int pwidth, pheight;

    int mutable_expression = 1;

    fprintf(stderr, "started as %s\n", name);
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

    status   = STATUS_SUCCESS;
    run_mode = param[0].data.d_int32;

    image_id = param[1].data.d_int32;
    layer_id = gimp_image_get_active_layer(image_id);

    values[0].type = PARAM_STATUS;
    values[0].data.d_status = status;

    *nreturn_vals = 1;
    *return_vals = values;

    /* Get the active drawable info */

    input_drawables[0].drawable = gimp_drawable_get(param[2].data.d_drawable);
    input_drawables[0].bpp = gimp_drawable_bpp(input_drawables[0].drawable->id);
    input_drawables[0].row = input_drawables[0].col = -1;
    input_drawables[0].tile = 0;
    input_drawables[0].fast_image_source = 0;
    input_drawables[0].used = 1;

    outputBPP = input_drawables[0].bpp;

    tile_width = gimp_tile_width();
    tile_height = gimp_tile_height();

    img_width = gimp_drawable_width(input_drawables[0].drawable->id);
    img_height = gimp_drawable_height(input_drawables[0].drawable->id);

    gimp_drawable_mask_bounds(input_drawables[0].drawable->id, &sel_x1, &sel_y1, &sel_x2, &sel_y2);

    originX = sel_x1;
    originY = sel_y1;

    sel_width  = sel_x2 - sel_x1;
    sel_height = sel_y2 - sel_y1;

    cen_x = (double) (sel_x2 - 1 + sel_x1) / 2.0;
    cen_y = (double) (sel_y2 - 1 + sel_y1) / 2.0;

    xhsiz = (double) (sel_width - 1) / 2.0;
    yhsiz = (double) (sel_height - 1) / 2.0;

    if (xhsiz < yhsiz) {
	scale_x = yhsiz / xhsiz;
	scale_y = 1.0;
    } else if (xhsiz > yhsiz) {
	scale_x = 1.0;
	scale_y = xhsiz / yhsiz;
    } else {
	scale_x = 1.0;
	scale_y = 1.0;
    } /* else */

    radius  = MAX(xhsiz, yhsiz);
    radius2 = radius * radius;

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
    init_internals();
    init_macros();
    init_noise();

    /* See how we will run */

    switch (run_mode) {
	case RUN_INTERACTIVE:
	    /* Possibly retrieve data */

	    gimp_get_data(name, &mmvals);

	    /* Get information from the dialog */

	    update_gradient();

	    if (!mathmap_dialog(mutable_expression))
		return;

	    break;

	case RUN_NONINTERACTIVE:
	    /* Make sure all the arguments are present */

	    if (nparams != 5)
		status = STATUS_CALLING_ERROR;

	    if (status == STATUS_SUCCESS)
	    {
		expression_copy(mmvals.expression, param[3].data.d_string);
		mmvals.flags = param[4].data.d_int32;
	    }

	    break;

	case RUN_WITH_LAST_VALS:
	    /* Possibly retrieve data */

	    gimp_get_data(name, &mmvals);
	    break;

	default:
	    break;
    } /* switch */

    /* Mathmap the image */

    if ((status == STATUS_SUCCESS)
	&& (gimp_drawable_color(input_drawables[0].drawable->id)
	    || gimp_drawable_gray(input_drawables[0].drawable->id)))
    {
	intersamplingEnabled = mmvals.flags & FLAG_INTERSAMPLING;
	oversamplingEnabled = mmvals.flags & FLAG_OVERSAMPLING;
	animationEnabled = mmvals.flags & FLAG_ANIMATION;

	update_gradient();

	/* Set the tile cache size */

	gimp_tile_cache_ntiles((input_drawables[0].drawable->width + gimp_tile_width() - 1)
			       / gimp_tile_width());

	/* Run! */

	if (animationEnabled)
	{
	    int frameNum;

	    for (frameNum = 0; frameNum < mmvals.frames; ++frameNum)
	    {
		gint32 layer;
		char layerName[100];

		currentT = (double)frameNum / (double)mmvals.frames;
		layer = mathmap_layer_copy(layer_id);
		sprintf(layerName, "Frame %d", frameNum + 1);
		gimp_layer_set_name(layer, layerName);
		output_drawable = gimp_drawable_get(layer);
		mathmap(frameNum);
		gimp_image_add_layer(image_id, layer, 0);
	    }
	}
	else
	{
	    currentT = mmvals.param_t;
	    output_drawable = input_drawables[0].drawable;
	    mathmap(-1);
	}

	/* If run mode is interactive, flush displays */

	if (run_mode != RUN_NONINTERACTIVE)
	    gimp_displays_flush();

	/* Store data */

	if (run_mode == RUN_INTERACTIVE)
	    gimp_set_data(name, &mmvals, sizeof(mathmap_vals_t));
    } else if (status == STATUS_SUCCESS)
	status = STATUS_EXECUTION_ERROR;

    values[0].data.d_status = status;

    gimp_drawable_detach(input_drawables[0].drawable);
} /* run */


/*****/

static internal_t *xy_internal = 0, *ra_internal = 0;

inline void
calc_ra (void)
{
    if (ra_internal->is_used)
    {
	double x = currentX,
	    y = currentY;

	currentR = sqrt(x * x + y * y);
	if (currentR == 0.0)
	    currentA = 0.0;
	else
	    currentA = acos(x / currentR) * 180 / M_PI;

	if (y < 0)
	    currentA = 360 - currentA;
    }
}

static void
init_internals (void)
{
    xy_internal = register_internal("xy", xy_tag_number, 2);
    ra_internal = register_internal("ra", ra_tag_number, 2);
    register_internal("t", nil_tag_number, 1);
    register_internal("XY", xy_tag_number, 2);
    register_internal("WH", xy_tag_number, 2);
    register_internal("R", nil_tag_number, 1);
}

static void
update_image_internals (void)
{
    internal_t *internal;
    tuple_info_t dummy;

    internal = lookup_internal("t", &dummy);
    internal->value.data[0] = currentT;

    internal = lookup_internal("XY", &dummy);
    internal->value.data[0] = imageX;
    internal->value.data[1] = imageY;
    
    internal = lookup_internal("WH", &dummy);
    internal->value.data[0] = imageW;
    internal->value.data[1] = imageH;
    
    internal = lookup_internal("R", &dummy);
    internal->value.data[0] = imageR;
}

static void
update_pixel_internals (void)
{
    xy_internal->value.data[0] = currentX;
    xy_internal->value.data[1] = currentY;

    ra_internal->value.data[0] = currentR;
    ra_internal->value.data[1] = currentA;
}

/*****/

static gint32 
mathmap_layer_copy(gint32 layerID)
{
    GParam *return_vals;
    int nreturn_vals;
    gint32 nlayer;

    return_vals = gimp_run_procedure ("gimp_layer_copy", 
				      &nreturn_vals,
				      PARAM_LAYER, layerID,
				      PARAM_INT32, TRUE,
				      PARAM_END);
 
    if (return_vals[0].data.d_status == STATUS_SUCCESS)
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

    samples = gimp_gradients_sample_uniform(num_gradient_samples);

    for (i = 0; i < num_gradient_samples; ++i)
	gradient_samples[i] = color_to_tuple(samples[i * 4 + 0], samples[i * 4 + 1],
					     samples[i * 4 + 2], samples[i * 4 + 3]);
}

/*****/

static int
generate_code (void)
{
    static int result;

    result = 1;

    if (expression_changed)
    {
	if (run_mode == RUN_INTERACTIVE && expression_entry != 0)
	    dialog_text_update();

	theExprtree = 0;
	usesRA = 0;

	untag_uservals();

	DO_JUMP_CODE {
	    clear_all_variables();
	    internals_clear_used();
	    scanFromString(mmvals.expression);
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

	    expression_changed = 0;
	} WITH_JUMP_HANDLER {
	    gimp_message(error_string);
	    make_empty_postfix();

	    result = 0;
	} END_JUMP_HANDLER;

	clear_untagged_uservals();
	untag_uservals();
	update_userval_table();
    }

    update_image_internals();

    return result;
}

/*****/

void
write_tuple_to_pixel (tuple_t *tuple, guchar *dest)
{
    float redf,
	greenf,
	bluef,
	alphaf;

    tuple_to_color(tuple, &redf, &greenf, &bluef, &alphaf);

    if (outputBPP == 1 || outputBPP == 2)
	dest[0] = (0.299 * redf + 0.587 * greenf + 0.114 * bluef) * 255;
    else if (outputBPP == 3 || outputBPP == 4)
    {
	dest[0] = redf * 255;
	dest[1] = greenf * 255;
	dest[2] = bluef * 255;
    }
    else
	assert(0);

    if (outputBPP == 2 || outputBPP == 4)
	dest[outputBPP - 1] = alphaf * 255;
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
alloc_input_drawable (GDrawable *drawable)
{
    int i;

    for (i = 0; i < MAX_INPUT_DRAWABLES; ++i)
	if (!input_drawables[i].used)
	    break;
    if (i == MAX_INPUT_DRAWABLES)
	return -1;

    input_drawables[i].drawable = drawable;
    input_drawables[i].bpp = gimp_drawable_bpp(drawable->id);
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

GDrawable*
get_input_drawable (int index)
{
    assert(input_drawables[index].used);

    return input_drawables[index].drawable;
}

/*****/

static void
mathmap (int frame_num)
{
    GPixelRgn dest_rgn;
    gpointer  pr;
    gint      progress, max_progress;
    guchar   *dest_row;
    guchar   *dest;
    gint      row, col;
    int i;
    gchar progress_info[30];

    previewing = 0;

    num_ops = 0;

    outputBPP = gimp_drawable_bpp(output_drawable->id);

    if (generate_code())
    {
	/* Initialize pixel region */

	gimp_pixel_rgn_init(&dest_rgn, output_drawable, sel_x1, sel_y1, sel_width, sel_height,
			    TRUE, TRUE);

	imageWidth = sel_width;
	imageW = imageWidth;
	imageHeight = sel_height;
	imageH = imageHeight;

	middleX = imageWidth / 2.0;
	middleY = imageHeight / 2.0;

	if (middleX > imageWidth - middleX)
	    imageX = middleX;
	else
	    imageX = imageWidth - middleX;

	if (middleY > imageHeight - middleY)
	    imageY = middleY;
	else
	    imageY = imageHeight - middleY;
    
	imageR = sqrt(imageX * imageX + imageY * imageY);

	progress     = 0;
	max_progress = sel_width * sel_height;

	if (frame_num >= 0)
	    sprintf(progress_info, "Mathmapping frame %d...", frame_num + 1);
	else
	    strcpy(progress_info, "Mathmapping...");
	gimp_progress_init(progress_info);

	for (pr = gimp_pixel_rgns_register(1, &dest_rgn);
	     pr != NULL; pr = gimp_pixel_rgns_process(pr))
	{
	    if (oversamplingEnabled)
	    {
		unsigned char *line1,
		    *line2,
		    *line3;

		dest_row = dest_rgn.data;

		line1 = (unsigned char*)malloc((sel_width + 1) * outputBPP);
		line2 = (unsigned char*)malloc(sel_width * outputBPP);
		line3 = (unsigned char*)malloc((sel_width + 1) * outputBPP);

		for (col = 0; col <= dest_rgn.w; ++col)
		{
		    currentX = col + dest_rgn.x - sel_x1 - middleX;
		    currentY = -(0.0 + dest_rgn.y - sel_y1 - middleY);
		    calc_ra();
		    update_pixel_internals();
		    write_tuple_to_pixel(EVAL_EXPR(), line1 + col * outputBPP);
		}

		for (row = 0; row < dest_rgn.h; ++row)
		{
		    dest = dest_row;

		    for (col = 0; col < dest_rgn.w; ++col)
		    {
			currentX = col + dest_rgn.x - sel_x1 + 0.5 - middleX;
			currentY = -(row + dest_rgn.y - sel_y1 + 0.5 - middleY);
			calc_ra();
			update_pixel_internals();
			write_tuple_to_pixel(EVAL_EXPR(), line2 + col * outputBPP);
		    }
		    for (col = 0; col <= dest_rgn.w; ++col)
		    {
			currentX = col + dest_rgn.x - sel_x1 - middleX;
			currentY = -(row + dest_rgn.y - sel_y1 + 1.0 - middleY);
			calc_ra();
			update_pixel_internals();
			write_tuple_to_pixel(EVAL_EXPR(), line3 + col * outputBPP);
		    }
	    
		    for (col = 0; col < dest_rgn.w; ++col)
		    {
			for (i = 0; i < outputBPP; ++i)
			    dest[i] = (line1[col*outputBPP+i]
				       + line1[(col+1)*outputBPP+i]
				       + 2*line2[col*outputBPP+i]
				       + line3[col*outputBPP+i]
				       + line3[(col+1)*outputBPP+i]) / 6;
			dest += outputBPP;
		    }

		    memcpy(line1, line3, (imageWidth + 1) * outputBPP);

		    dest_row += dest_rgn.rowstride;
		}
	    }
	    else
	    {
		dest_row = dest_rgn.data;

		for (row = dest_rgn.y; row < (dest_rgn.y + dest_rgn.h); row++)
		{
		    dest = dest_row;

		    for (col = dest_rgn.x; col < (dest_rgn.x + dest_rgn.w); col++)
		    {
			currentX = col - sel_x1 - middleX;
			currentY = -(row - sel_y1 - middleY);
			calc_ra();
			update_pixel_internals();
			write_tuple_to_pixel(EVAL_EXPR(), dest);
			dest += outputBPP;
		    }
		
		    dest_row += dest_rgn.rowstride;
		}
	    }

	    /* Update progress */
	    progress += dest_rgn.w * dest_rgn.h;
	    gimp_progress_update((double) progress / max_progress);
	}

	unref_tiles();

	gimp_drawable_flush(output_drawable);
	gimp_drawable_merge_shadow(output_drawable->id, TRUE);
	gimp_drawable_update(output_drawable->id, sel_x1, sel_y1, sel_width, sel_height);

#ifndef USE_CGEN
	fprintf(stderr, "executed %d instructions\n", num_ops);
#endif
    }
} /* mathmap */

/*****/

void
mathmap_get_pixel(int drawable_index, int x, int y, guchar *pixel)
{
    gint newcol, newrow;
    gint newcoloff, newrowoff;
    guchar *p;
    int i;
    input_drawable_t *drawable;

    if (drawable_index < 0 || drawable_index >= MAX_INPUT_DRAWABLES || !input_drawables[drawable_index].used
	|| x < 0 || x >= img_width
	|| y < 0 || y >= img_height)
    {
	for (i = 0; i < outputBPP; ++i)
	    pixel[i] = edge_color[i];
	return;
    }

    drawable = &input_drawables[drawable_index];

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
}

void
mathmap_get_fast_pixel(int drawable_index, int x, int y, guchar *pixel)
{
    input_drawable_t *drawable;

    if (drawable_index < 0 || drawable_index >= MAX_INPUT_DRAWABLES || !input_drawables[drawable_index].used
	|| x < 0 || x >= preview_width
	|| y < 0 || y >= preview_height)
    {
	int i;

	for (i = 0; i < outputBPP; ++i)
	    pixel[i] = edge_color[i];
	return;
    }

    drawable = &input_drawables[drawable_index];

    if (drawable->fast_image_source == 0)
	build_fast_image_source(drawable);

    memcpy(pixel, drawable->fast_image_source + (x + y * preview_width) * 4, 4);
}

/*****/

static void
build_fast_image_source (input_drawable_t *drawable)
{
    guchar *p;
    int x, y;

    assert(drawable->fast_image_source == 0);

    p = drawable->fast_image_source = g_malloc(preview_width * preview_height * 4);

    for (y = 0; y < preview_height; ++y)
    {
	for (x = 0; x < preview_width; ++x)
	{
	    mathmap_get_pixel(drawable - input_drawables,
			      sel_x1 + x * sel_width / preview_width,
			      sel_y1 + y * sel_height / preview_height, p);
	    p += 4;
	}
    }
}

/*****/

static GtkWidget*
tree_from_lisp_object (GtkWidget *root_item, lisp_object_t *obj)
{
    GtkWidget *tree = gtk_tree_new();

    if (root_item != 0)
	gtk_tree_item_set_subtree(GTK_TREE_ITEM(root_item), tree);

    for (; lisp_type(obj) != LISP_TYPE_NIL; obj = lisp_cdr(obj))
    {
	lisp_object_t *vars[2];
	GtkWidget *item = 0;

	assert(lisp_type(obj) == LISP_TYPE_CONS);

	if (lisp_match_string("(group #?(string) . #?(list))", lisp_car(obj), vars))
	{
	    item = gtk_tree_item_new_with_label(lisp_string(vars[0]));
	    gtk_tree_append(GTK_TREE(tree), item);
	    gtk_widget_show(item);
	    tree_from_lisp_object(item, vars[1]);
	}
	else if (lisp_match_string("(expression #?(string) #?(string))", lisp_car(obj), vars))
	{
	    item = gtk_tree_item_new_with_label(lisp_string(vars[0]));
	    gtk_tree_append(GTK_TREE(tree), item);
	    gtk_widget_show(item);
	    gtk_object_set_user_data(GTK_OBJECT(item),
				     strcpy((char*)malloc(strlen(lisp_string(vars[1])) + 1),
					    lisp_string(vars[1])));
	}
	else
	    assert(0);
    }

    gtk_widget_show(tree);

    if (root_item != 0)
	gtk_tree_item_expand(GTK_TREE_ITEM(root_item));

    return tree;
}

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

/*****/

static void
update_userval_table (void)
{
    if (uservalues_table != 0)
    {
	gtk_container_remove(GTK_CONTAINER(GTK_BIN(uservalues_scrolled_window)->child), uservalues_table);
	uservalues_table = 0;
    }

    uservalues_table = make_userval_table();

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

static gint
mathmap_dialog (int mutable_expression)
{
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
    gint        argc,
	position = 0;
    gchar     **argv;
    guchar     *color_cube;

    argc    = 1;
    argv    = g_new(gchar *, 1);
    argv[0] = g_strdup("mathmap");

    gtk_init(&argc, &argv);

    gtk_preview_set_gamma(gimp_gamma());
    gtk_preview_set_install_cmap(gimp_install_cmap());
    color_cube = gimp_color_cube();
    gtk_preview_set_color_cube(color_cube[0], color_cube[1], color_cube[2], color_cube[3]);

    gtk_widget_set_default_visual(gtk_preview_get_visual());
    gtk_widget_set_default_colormap(gtk_preview_get_cmap());

    wint.wimage = g_malloc(preview_width * preview_height * 3 * sizeof(guchar));

    dialog = gtk_dialog_new();
    gtk_window_set_title(GTK_WINDOW(dialog), "MathMap");
    gtk_window_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);
    gtk_container_border_width(GTK_CONTAINER(dialog), 0);
    gtk_signal_connect(GTK_OBJECT(dialog), "destroy",
		       (GtkSignalFunc) dialog_close_callback,
		       NULL);

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

    button = gtk_button_new_with_label("Preview");
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

                /* Intersampling */

		toggle = gtk_check_button_new_with_label("Intersampling");
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle),
					    mmvals.flags & FLAG_INTERSAMPLING);
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_intersampling_update, 0);
		gtk_widget_show(toggle);

		/* Oversampling */
	    
		toggle = gtk_check_button_new_with_label("Oversampling");
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle),
					    mmvals.flags & FLAG_OVERSAMPLING);
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_oversampling_update, 0);
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

	        toggle = gtk_check_button_new_with_label("Auto Preview");
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle), auto_preview);
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_auto_preview_update, 0);
		gtk_widget_show(toggle);

	        /* Fast Preview */

		toggle = gtk_check_button_new_with_label("Fast Preview");
		gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(toggle), fast_preview);
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_fast_preview_update, 0);
		gtk_widget_show(toggle);

	    /* Edge Behaviour */

	    table = gtk_table_new(2, 3, FALSE);
	    gtk_container_border_width(GTK_CONTAINER(table), 6);
	    gtk_table_set_row_spacings(GTK_TABLE(table), 4);

	    frame = gtk_frame_new("Edge Behaviour");
	    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
	    gtk_container_add(GTK_CONTAINER(frame), table);
	    gtk_table_attach(GTK_TABLE(middle_table), frame, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);

	    gtk_widget_show(table);
	    gtk_widget_show(frame);

	        /* Color */

	        toggle = gtk_radio_button_new_with_label(edge_group, "Color");
		edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_edge_behaviour_update, &edge_behaviour_color);
		gtk_widget_show(toggle);

		edge_color_well = color_well_new();
		gtk_signal_connect(GTK_OBJECT(edge_color_well), "color-changed",
				   (GtkSignalFunc)dialog_edge_color_changed, 0);
		dialog_edge_color_set();
		gtk_widget_show(edge_color_well);
		gtk_table_attach(GTK_TABLE(table), edge_color_well, 1, 2, 0, 1, GTK_FILL, 0, 0, 0);

	        /* Wrap */

	        toggle = gtk_radio_button_new_with_label(edge_group, "Wrap");
		edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_edge_behaviour_update, &edge_behaviour_wrap);
		gtk_widget_show(toggle);

	        /* Reflect */

	        toggle = gtk_radio_button_new_with_label(edge_group, "Reflect");
		edge_group = gtk_radio_button_group(GTK_RADIO_BUTTON(toggle));
		gtk_table_attach(GTK_TABLE(table), toggle, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);
		gtk_signal_connect(GTK_OBJECT(toggle), "toggled",
				   (GtkSignalFunc)dialog_edge_behaviour_update, &edge_behaviour_reflect);
		gtk_widget_show(toggle);

	    /* Animation */
	    
	    table = gtk_table_new(3, 1, FALSE);
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
		toggle = gtk_check_button_new_with_label("Animate");
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

		label = gtk_label_new("Frames");
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

		/* t */

		t_table = gtk_table_new(1, 2, FALSE);
		gtk_table_set_col_spacings(GTK_TABLE(t_table), 4);
		gtk_table_attach(GTK_TABLE(table), t_table, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);

		label = gtk_label_new("Parameter t");
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

	label = gtk_label_new("Settings");
	gtk_widget_show(label);
	gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), middle_table, label, label);

        /* Expression */

	if (mutable_expression)
	{
	    table = gtk_hbox_new(FALSE, 0);
	    gtk_widget_show(table);

	    label = gtk_label_new("Expression");
	    gtk_widget_show(label);
	    gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), table, label, label);

	    expression_entry = gtk_text_new(NULL, NULL);
	    gtk_signal_connect(GTK_OBJECT(expression_entry), "changed",
			       (GtkSignalFunc)dialog_text_changed,
			       (gpointer)NULL);
	    gtk_text_set_editable(GTK_TEXT(expression_entry), TRUE);
	    gtk_box_pack_start(GTK_BOX(table), expression_entry, TRUE, TRUE, 0);
	    gtk_widget_show(expression_entry);
	    /* gtk_text_freeze(GTK_TEXT(expression_entry)); */
	    gtk_widget_realize(expression_entry);
	    /* gtk_text_thaw(GTK_TEXT(expression_entry)); */
	    gtk_editable_insert_text(GTK_EDITABLE(expression_entry), mmvals.expression,
				     strlen(mmvals.expression), &position);

	    vscrollbar = gtk_vscrollbar_new(GTK_TEXT(expression_entry)->vadj);
	    gtk_box_pack_start(GTK_BOX(table), vscrollbar, FALSE, FALSE, 0);
	    gtk_widget_show (vscrollbar);
	}

	/* User Values */

	uservalues_scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(uservalues_scrolled_window),
					GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_show(uservalues_scrolled_window);

	uservalues_table = 0;

	label = gtk_label_new("User Values");
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
	    gtk_signal_connect(GTK_OBJECT(root_tree), "selection_changed",
			       (GtkSignalFunc)dialog_tree_changed,
			       (gpointer)NULL);
#if GTK_MAJOR_VERSION < 1 || (GTK_MAJOR_VERSION == 1 && GTK_MINOR_VERSION < 1)
	    gtk_container_add(GTK_CONTAINER(table), root_tree);
#else
	    gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(table), root_tree);
#endif
	    gtk_tree_set_selection_mode(GTK_TREE(root_tree), GTK_SELECTION_BROWSE);
	    gtk_tree_set_view_lines(GTK_TREE(root_tree), FALSE);
	    gtk_tree_set_view_mode(GTK_TREE(root_tree), FALSE);
	    gtk_widget_show(root_tree);

	    label = gtk_label_new("Examples");
	    gtk_widget_show(label);
	    gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), table, label, label);
	}

    /* Buttons */

    gtk_container_border_width(GTK_CONTAINER(GTK_DIALOG(dialog)->action_area), 6);

    button = gtk_button_new_with_label("OK");
    GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       (GtkSignalFunc) dialog_ok_callback,
		       dialog);
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area), button, TRUE, TRUE, 0);
    gtk_widget_grab_default(button);
    gtk_widget_show(button);

    button = gtk_button_new_with_label("Cancel");
    GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       (GtkSignalFunc) dialog_cancel_callback,
		       dialog);
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area), button, TRUE, TRUE, 0);
    gtk_widget_show(button);

    button = gtk_button_new_with_label("Help");
    GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       (GtkSignalFunc) dialog_help_callback,
		       dialog);
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area), button, TRUE, TRUE, 0);
    gtk_widget_show(button);

    button = gtk_button_new_with_label("About");
    GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       (GtkSignalFunc) dialog_about_callback,
		       dialog);
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area), button, TRUE, TRUE, 0);
    gtk_widget_show(button);

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
dialog_update_preview(void)
{
    double  left, right, bottom, top;
    double  dx, dy;
    int     x, y;
    double  scale_x, scale_y;
    guchar *p_ul, *p_lr, *p;
    gint check,check_0,check_1; 

    update_uservals();

    previewing = fast_preview;

    intersamplingEnabled = mmvals.flags & FLAG_INTERSAMPLING;
    oversamplingEnabled = mmvals.flags & FLAG_OVERSAMPLING;

    currentT = mmvals.param_t;

    left   = sel_x1;
    right  = sel_x2 - 1;
    bottom = sel_y2 - 1;
    top    = sel_y1;

    dx = (right - left) / (preview_width - 1);
    dy = (bottom - top) / (preview_height - 1);

    scale_x = (double) (preview_width - 1) / (right - left);
    scale_y = (double) (preview_height - 1) / (bottom - top);

    p_ul = wint.wimage;
    p_lr = wint.wimage + 3 * (preview_width * preview_height - 1);

    imageWidth = sel_width;
    imageW = imageWidth;
    imageHeight = sel_height;
    imageH = imageHeight;

    middleX = imageWidth / 2.0;
    middleY = imageHeight / 2.0;

    if (middleX > imageWidth - middleX)
	imageX = middleX;
    else
	imageX = imageWidth - middleX;

    if (middleY > imageHeight - middleY)
	imageY = middleY;
    else
	imageY = imageHeight - middleY;
    
    imageR = sqrt(imageX * imageX + imageY * imageY);

    if (generate_code())
    {
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
		tuple_t *result;
		float redf,
		    greenf,
		    bluef,
		    alphaf;

		currentX = x * imageWidth / preview_width - middleX;
		currentY = -(y * imageHeight / preview_height - middleY);
		calc_ra();
		update_pixel_internals();
		result = EVAL_EXPR();
		tuple_to_color(result, &redf, &greenf, &bluef, &alphaf);

		if (input_drawables[0].bpp < 2)
		    redf = greenf = bluef = 0.299 * redf + 0.587 * greenf + 0.114 * bluef;

		p_ul[0] = redf * 255;
		p_ul[1] = greenf * 255;
		p_ul[2] = bluef * 255;

		if (outputBPP == 2 || outputBPP == 4)
		{
		    if (((x) / CHECK_SIZE) & 1)
			check = check_0;
		    else
			check = check_1;
		    p_ul[0] = check + (p_ul[0] - check) * alphaf;
		    p_ul[1] = check + (p_ul[1] - check) * alphaf;
		    p_ul[2] = check + (p_ul[2] - check) * alphaf;
		}

		p_ul += 3;
	    }
	}

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
dialog_scale_update(GtkAdjustment *adjustment, gint *value)
{
    *value = (gint)adjustment->value;
} /* dialog_scale_update */


/*****/

static void
dialog_t_update(GtkAdjustment *adjustment, gfloat *value)
{
    *value = (gfloat)adjustment->value;

    if (auto_preview)
	dialog_update_preview();
} /* dialog_scale_update */


/*****/

static void
dialog_text_changed (void)
{
    expression_changed = 1;
}

/*****/

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

/*****/

static void
dialog_oversampling_update (GtkWidget *widget, gpointer data)
{
    mmvals.flags &= ~FLAG_OVERSAMPLING;

    if (GTK_TOGGLE_BUTTON(widget)->active)
	mmvals.flags |= FLAG_OVERSAMPLING;
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
    if (edge_behaviour_mode == edge_behaviour_color)
    {
	gtk_widget_set_sensitive(edge_color_well, 1);
    }
    else
    {
	gtk_widget_set_sensitive(edge_color_well, 0);
    }

    if (auto_preview)
	dialog_update_preview();
}

static void
dialog_edge_color_set (void)
{
    gdouble color[4];
    int i;

    for (i = 0; i < 4; ++i)
	color[i] = edge_color[i] / 255.0;

    color_well_set_color(COLOR_WELL(edge_color_well), color);
}

static void
dialog_edge_color_changed (ColorWell *color_well, gpointer data)
{
    gdouble color[4];
    int i;

    color_well_get_color(color_well, color);
    for (i = 0; i < 4; ++i)
	edge_color[i] = color[i] * 255.0;
    if (auto_preview)
	dialog_update_preview();
}

/*****/

static void
dialog_intersampling_update (GtkWidget *widget, gpointer data)
{
    mmvals.flags &= ~FLAG_INTERSAMPLING;

    if (GTK_TOGGLE_BUTTON(widget)->active)
	mmvals.flags |= FLAG_INTERSAMPLING;

    expression_changed = 1;

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
dialog_preview_callback (GtkWidget *widget, gpointer data)
{
    update_gradient();
    dialog_update_preview();
}

/*****/

static void
dialog_close_callback (GtkWidget *widget, gpointer data)
{
    gtk_main_quit();
} /* dialog_close_callback */


/*****/

static void
dialog_ok_callback (GtkWidget *widget, gpointer data)
{
    if (generate_code())
    {
	wint.run = TRUE;
	gtk_widget_destroy(GTK_WIDGET(data));
    }
} /* dialog_ok_callback */


/*****/

static void
dialog_cancel_callback (GtkWidget *widget, gpointer data)
{
    gtk_widget_destroy(GTK_WIDGET(data));
} /* dialog_cancel_callback */

/*****/

static void
dialog_help_callback (GtkWidget *widget, gpointer data)
{
    char *proc_blurb, *proc_help, *proc_author, *proc_copyright, *proc_date;
    int proc_type, nparams, nreturn_vals;
    GParamDef *params, *return_vals;
    gint baz;

    if (gimp_query_procedure ("extension_web_browser",
			      &proc_blurb, &proc_help, 
			      &proc_author, &proc_copyright, &proc_date,
			      &proc_type, &nparams, &nreturn_vals,
			      &params, &return_vals)) 
	gimp_run_procedure ("extension_web_browser", &baz,
			    PARAM_INT32, RUN_NONINTERACTIVE,
			    PARAM_STRING, MATHMAP_MANUAL_URL,
			    PARAM_INT32, 1,
			    PARAM_END);
    else 
    {
	gchar *message = g_strdup_printf("See %s", MATHMAP_MANUAL_URL);

	gimp_message(message);
	g_free(message);
    }                                            
} /* dialog_help_callback */

/*****/

static void
dialog_about_callback (GtkWidget *widget, gpointer data)
{
    gimp_message("MathMap " MATHMAP_VERSION "\n"
		 "written by\n"
		 "Mark Probst <schani@complang.tuwien.ac.at>");
} /* dialog_about_callback */

/*****/

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
