/* The GIMP -- an image manipulation program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * MathMap plug-in --- generate an image by means of a mathematical expression
 * Copyright (C) 1997-1999 Mark Probst
 * schani@unix.cslab.tuwien.ac.at
 *
 * Plug-In structure based on:
 *   Whirl plug-in --- distort an image into a whirlpool
 *   Copyright (C) 1997 Federico Mena Quintero
 *   federico@nuclecu.unam.mx
 *
 * Version 0.8
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


#include <sys/param.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <proplist.h>

#include <gtk/gtk.h>
#include <libgimp/gimp.h>

#include "exprtree.h"
#include "builtins.h"
#include "postfix.h"
#include "tags.h"
#include "scanner.h"
#include "vars.h"
#include "internals.h"
#include "macros.h"
#include "jump.h"
#include "mathmap.h"
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

#define USER_CURVE_POINTS       1024

#define NUM_GRADIENT_SAMPLES    1024

/***** Types *****/

typedef struct {
    gchar expression[MAX_EXPRESSION_LENGTH];
    gint flags;
    gint frames;
    gfloat param_t;
    gint num_curve_points;
    gint16 curve_points[USER_CURVE_POINTS];
} mathmap_vals_t;

typedef struct {
	GtkWidget *preview;
	guchar    *image;
	guchar    *wimage;

	gint run;
} mathmap_interface_t;


/***** Prototypes *****/

static void query(void);
static void run(char    *name,
		int      nparams,
		GParam  *param,
		int     *nreturn_vals,
		GParam **return_vals);

static void expression_copy (gchar *dest, gchar *src);

static void mathmap (int frame_num);
void   mathmap_get_pixel(int x, int y, guchar *pixel);
static gint32 mathmap_layer_copy(gint32 layerID);

extern int mmparse (void);

static void build_fast_image_source (void);
static void build_preview_source_image(void);

static void update_gradient (void);
static gint mathmap_dialog(void);
static void dialog_update_preview(void);
static void dialog_scale_update(GtkAdjustment *adjustment, gint *value);
static void dialog_t_update(GtkAdjustment *adjustment, gfloat *value);
static void dialog_text_changed (void);
static void dialog_text_update (void);
static void dialog_intersampling_update (GtkWidget *widget, gpointer data);
static void dialog_oversampling_update (GtkWidget *widget, gpointer data);
static void dialog_auto_preview_update (GtkWidget *widget, gpointer data);
static void dialog_fast_preview_update (GtkWidget *widget, gpointer data);
static void dialog_animation_update (GtkWidget *widget, gpointer data);
static void dialog_preview_callback (GtkWidget *widget, gpointer data);
static void dialog_close_callback(GtkWidget *widget, gpointer data);
static void dialog_ok_callback(GtkWidget *widget, gpointer data);
static void dialog_cancel_callback(GtkWidget *widget, gpointer data);
static void dialog_help_callback(GtkWidget *widget, gpointer data);
static void dialog_tree_changed (GtkTree *tree);

/***** Variables *****/

GPlugInInfo PLUG_IN_INFO = {
	NULL,   /* init_proc */
	NULL,   /* quit_proc */
	query,  /* query_proc */
	run     /* run_proc */
}; /* PLUG_IN_INFO */


static mathmap_vals_t mmvals = {
	DEFAULT_EXPRESSION,      /* expression */
	FLAG_INTERSAMPLING,      /* flags */
	DEFAULT_NUMBER_FRAMES,   /* frames */
	0.0,                     /* t */
	USER_CURVE_POINTS        /* number of curve points */
}; /* mmvals */

static mathmap_interface_t wint = {
	NULL,  /* preview */
	NULL,  /* image */
	NULL,  /* wimage */
	FALSE  /* run */
}; /* wint */

static gint32 image_id;
static gint32 layer_id;
static GDrawable *input_drawable;
static GDrawable *output_drawable;

static gint   tile_width, tile_height;
static gint   img_width, img_height;
gint   sel_x1, sel_y1, sel_x2, sel_y2;
gint   sel_width, sel_height;
gint   preview_width, preview_height;
static GTile *the_tile = NULL;

static double cen_x, cen_y;
static double scale_x, scale_y;
static double radius, radius2;

guchar *fast_image_source = 0;

char error_string[1024];

GtkWidget *expression_entry = 0,
    *frame_table,
    *t_table,
    *user_curve;

exprtree *theExprtree = 0;
int imageWidth,
    imageHeight,
    wholeImageWidth,
    wholeImageHeight,
    originX,
    originY,
    inputBPP,
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
double user_curve_values[USER_CURVE_POINTS];
int num_gradient_samples = NUM_GRADIENT_SAMPLES;
tuple_t gradient_samples[NUM_GRADIENT_SAMPLES];
int user_curve_points = USER_CURVE_POINTS;

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

static void
query(void)
{
    static GParamDef args[] = {
	{ PARAM_INT32,      "run_mode",         "Interactive, non-interactive" },
	{ PARAM_IMAGE,      "image",            "Input image" },
	{ PARAM_DRAWABLE,   "drawable",         "Input drawable" },
	{ PARAM_STRING,     "expression",       "MathMap expression" },
	{ PARAM_INT32,      "flags",            "1: Intersampling 2: Oversampling 4: Animate" },
	{ PARAM_INT32,      "frames",           "Number of frames" },
	{ PARAM_FLOAT,      "param_t",          "The parameter t (if not animating)" },
	{ PARAM_INT32,      "num_curve_points", "Number of curve points" },
	{ PARAM_INT16ARRAY, "curve_points",     "Curve Points (range 0-1023)" }
    }; /* args */

    static GParamDef *return_vals  = NULL;
    static int        nargs        = sizeof(args) / sizeof(args[0]);
    static int        nreturn_vals = 0;

    gimp_install_procedure("plug_in_mathmap",
			   "Generate an image using a mathematical expression.",
			   "Generates an image by means of a mathematical expression. The expression "
			   "can also refer to the data of an original image. Thus, arbitrary "
			   "distortions can be constructed. Even animations can be generated.",
			   "Mark Probst",
			   "Mark Probst",
			   "January 1998, 0.5",
			   "<Image>/Filters/Generic/MathMap",
			   "RGB*, GRAY*",
			   PROC_PLUG_IN,
			   nargs,
			   nreturn_vals,
			   args,
			   return_vals);
} /* query */


/*****/

static void
run(char    *name,
    int      nparams,
    GParam  *param,
    int     *nreturn_vals,
    GParam **return_vals)
{
    static GParam values[1];

    GRunModeType  run_mode;
    GStatusType   status;
    double        xhsiz, yhsiz;
    int           pwidth, pheight;
    int i;

    status   = STATUS_SUCCESS;
    run_mode = param[0].data.d_int32;

    image_id = param[1].data.d_int32;
    layer_id = gimp_image_get_active_layer(image_id);

    values[0].type          = PARAM_STATUS;
    values[0].data.d_status = status;

    *nreturn_vals = 1;
    *return_vals  = values;

	/* Get the active drawable info */

    input_drawable = gimp_drawable_get(param[2].data.d_drawable);

    tile_width  = gimp_tile_width();
    tile_height = gimp_tile_height();

    img_width  = gimp_drawable_width(input_drawable->id);
    img_height = gimp_drawable_height(input_drawable->id);
    inputBPP = gimp_drawable_bpp(input_drawable->id);

    gimp_drawable_mask_bounds(input_drawable->id, &sel_x1, &sel_y1, &sel_x2, &sel_y2);

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

    /* See how we will run */

    switch (run_mode) {
	case RUN_INTERACTIVE:
	    /* Possibly retrieve data */

	    /*
	    fprintf(stderr, "mathmap starting with pid %d\n", getpid());
	    */

	    for (i = 0; i < USER_CURVE_POINTS; ++i)
		mmvals.curve_points[i] = i;

	    gimp_get_data("plug_in_mathmap", &mmvals);

	    /* Get information from the dialog */

	    build_fast_image_source();

	    update_gradient();

	    if (!mathmap_dialog())
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

	    gimp_get_data("plug_in_mathmap", &mmvals);
	    break;

	default:
	    break;
    } /* switch */

    /* Mathmap the image */

    if ((status == STATUS_SUCCESS)
	&& (gimp_drawable_color(input_drawable->id)
	    || gimp_drawable_gray(input_drawable->id)))
    {
	intersamplingEnabled = mmvals.flags & FLAG_INTERSAMPLING;
	oversamplingEnabled = mmvals.flags & FLAG_OVERSAMPLING;
	animationEnabled = mmvals.flags & FLAG_ANIMATION;

	update_gradient();

	/* Set the tile cache size */

	gimp_tile_cache_ntiles((input_drawable->width + gimp_tile_width() - 1)
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
	    output_drawable = input_drawable;
	    mathmap(-1);
	}

	/* If run mode is interactive, flush displays */

	if (run_mode != RUN_NONINTERACTIVE)
	    gimp_displays_flush();

	/* Store data */

	if (run_mode == RUN_INTERACTIVE)
	{
	    int i;

	    mmvals.num_curve_points = user_curve_points;
	    for (i = 0; i < user_curve_points; ++i)
		mmvals.curve_points[i] = user_curve_values[i] * 1023;

	    gimp_set_data("plug_in_mathmap", &mmvals, sizeof(mathmap_vals_t));
	}
    } else if (status == STATUS_SUCCESS)
	status = STATUS_EXECUTION_ERROR;

    values[0].data.d_status = status;

    gimp_drawable_detach(input_drawable);
} /* run */


/*****/

inline void
calc_ra (void)
{
    if (usesRA)
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

static void
update_curve (void)
{
    gfloat vector[USER_CURVE_POINTS];
    int i;

    gtk_curve_get_vector(GTK_CURVE(GTK_GAMMA_CURVE(user_curve)->curve), USER_CURVE_POINTS, vector);
    for (i = 0; i < USER_CURVE_POINTS; ++i)
	user_curve_values[i] = vector[i];
}

/*****/

static int
generate_code (void)
{
    static int result;

    result = 1;

    if (expression_changed)
    {
	dialog_text_update();

	theExprtree = 0;
	usesRA = 0;

	DO_JUMP_CODE {
	    clear_all_variables();
	    scanFromString(mmvals.expression);
	    mmparse();
	    endScanningFromString();

	    assert(theExprtree != 0);

	    if (theExprtree->result.number != rgba_tag_number || theExprtree->result.length != 4)
	    {
		sprintf(error_string, "The expression must have the result type rgba:4.");
		JUMP(0);
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

    dest[0] = redf * 255;
    if (outputBPP == 2)
	dest[1] = alphaf * 255;
    else if (outputBPP >= 3)
    {
	dest[1] = greenf * 255;
	dest[2] = bluef * 255;
	if (outputBPP == 4)
	    dest[3] = alphaf * 255;
    }
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

    generate_code();

    /* Initialize pixel region */

    gimp_pixel_rgn_init(&dest_rgn, output_drawable, sel_x1, sel_y1, sel_width, sel_height,
			TRUE, TRUE);

    imageWidth = sel_width;
    imageW = imageWidth;
    imageHeight = sel_height;
    imageH = imageHeight;
    wholeImageWidth = img_width;
    wholeImageHeight = img_height;

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
		currentY = 0.0 + dest_rgn.y - sel_y1 - middleY;
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
		    currentY = row + dest_rgn.y - sel_y1 + 0.5 - middleY;
		    calc_ra();
		    update_pixel_internals();
		    write_tuple_to_pixel(EVAL_EXPR(), line2 + col * outputBPP);
		}
		for (col = 0; col <= dest_rgn.w; ++col)
		{
		    currentX = col + dest_rgn.x - sel_x1 - middleX;
		    currentY = row + dest_rgn.y - sel_y1 + 1.0 - middleY;
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
		    currentX = col - sel_x1 - middleX; currentY = row - sel_y1 - middleY;
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

    if (the_tile != NULL) {
	gimp_tile_unref(the_tile, FALSE);
	the_tile = NULL;
    } /* if */

    gimp_drawable_flush(output_drawable);
    gimp_drawable_merge_shadow(output_drawable->id, TRUE);
    gimp_drawable_update(output_drawable->id, sel_x1, sel_y1, sel_width, sel_height);

    fprintf(stderr, "executed %d instructions\n", num_ops);
} /* mathmap */


/*****/

void
mathmap_get_pixel(int x, int y, guchar *pixel)
{
    static gint row  = -1;
    static gint col  = -1;

    gint    newcol, newrow;
    gint    newcoloff, newrowoff;
    guchar *p;
    int     i;

    if ((x < 0) || (x >= img_width) || (y < 0) || (y >= img_height)) {
	for (i = 0; i < outputBPP; ++i)
	    pixel[i] = 0;

	return;
    } /* if */

    newcol    = x / tile_width; /* The compiler should optimize this */
    newcoloff = x % tile_width;
    newrow    = y / tile_height;
    newrowoff = y % tile_height;

    if ((col != newcol) || (row != newrow) || (the_tile == NULL)) {
	if (the_tile != NULL)
	    gimp_tile_unref(the_tile, FALSE);

	the_tile = gimp_drawable_get_tile(input_drawable, FALSE, newrow, newcol);
	assert(the_tile != 0);
	gimp_tile_ref(the_tile);

	col = newcol;
	row = newrow;
    } /* if */

    p = the_tile->data + the_tile->bpp * (the_tile->ewidth * newrowoff + newcoloff);

    for (i = inputBPP; i; i--)
	*pixel++ = *p++;
    for (i = inputBPP; i < outputBPP; ++i)
	*pixel++ = 255;
}

/*****/

static void
build_fast_image_source (void)
{
    guchar *p;
    int x,
	y;

    p = fast_image_source = g_malloc(preview_width * preview_height * inputBPP);

    for (y = 0; y < preview_height; ++y)
    {
	for (x = 0; x < preview_width; ++x)
	{
	    mathmap_get_pixel(sel_x1 + x * sel_width / preview_width,
			      sel_y1 + y * sel_height / preview_height, p);
	    p += inputBPP;
	}
    }
}

/*****/

static void
build_preview_source_image(void)
{
    double  left, right, bottom, top;
    double  px, py;
    double  dx, dy;
    int     x, y;
    guchar *p;
    guchar  pixel[4];

    outputBPP = inputBPP;

    wint.image  = g_malloc(preview_width * preview_height * 3 * sizeof(guchar));
    wint.wimage = g_malloc(preview_width * preview_height * 3 * sizeof(guchar));

    left   = sel_x1;
    right  = sel_x2 - 1;
    bottom = sel_y2 - 1;
    top    = sel_y1;

    dx = (right - left) / (preview_width - 1);
    dy = (bottom - top) / (preview_height - 1);

    py = top;

    p = wint.image;

    for (y = 0; y < preview_height; y++)
    {
	px = left;

	for (x = 0; x < preview_width; x++)
	{
	    mathmap_get_pixel((int) px, (int) py, pixel);

	    if (inputBPP < 3)
	    {
		pixel[1] = pixel[0];
		pixel[2] = pixel[0];
	    } /* if */

	    *p++ = pixel[0];
	    *p++ = pixel[1];
	    *p++ = pixel[2];

	    px += dx;
	} /* for */

	py += dy;
    } /* for */
} /* build_preview_source_image */

/*****/

static GtkWidget*
tree_from_proplist (GtkWidget *root_item, proplist_t pl)
{
    GtkWidget *tree = gtk_tree_new();
    proplist_t keys;
    int length,
	i;

    if (root_item != 0)
	gtk_tree_item_set_subtree(GTK_TREE_ITEM(root_item), tree);

    assert(PLIsDictionary(pl));

    keys = PLGetAllDictionaryKeys(pl);
    length = PLGetNumberOfElements(keys);

    for (i = 0; i < length; ++i)
    {
	proplist_t key = PLGetArrayElement(keys, i),
	    value = PLGetDictionaryEntry(pl, key);
	GtkWidget *item;

	item = gtk_tree_item_new_with_label(PLGetString(key));
	gtk_tree_append(GTK_TREE(tree), item);
	gtk_widget_show(item);

	if (!PLIsString(value))
	    tree_from_proplist(item, value);
	else
	    gtk_object_set_user_data(GTK_OBJECT(item),
				     strcpy((char*)malloc(strlen(PLGetString(value)) + 1),
					    PLGetString(value)));
    }

    gtk_widget_show(tree);

    if (root_item != 0)
	gtk_tree_item_expand(GTK_TREE_ITEM(root_item));

    return tree;
}

static GtkWidget*
read_tree_from_rc (void)
{
    char filename[MAXPATHLEN + 1];
    proplist_t pl;
    GtkWidget *tree;

    strcpy(filename, getenv("HOME"));
    strcat(filename, "/" _GIMPDIR "/mathmaprc");

    pl = PLGetProplistWithPath(filename);

    if (pl == 0)
    {
	pl = PLGetProplistWithPath("/usr/local/share/gimp/mathmaprc");
	if (pl == 0)
	{
	    tree = gtk_tree_new();
	    gtk_widget_show(tree);
	    return tree;
	}
    }

    tree = tree_from_proplist(0, pl);

    return tree;
}


/*****/

static gint
mathmap_dialog(void)
{
    GtkWidget  *dialog;
    GtkWidget *top_table,
	*middle_table;
    GtkWidget *vbox;
    GtkWidget  *frame;
    GtkWidget  *table;
    GtkWidget  *button;
    GtkWidget  *label;
    GtkWidget  *toggle;
    GtkWidget  *alignment;
    GtkWidget *root_tree;
    GtkWidget *scale;
    GtkWidget *vscrollbar;
    GtkWidget *notebook;
    GtkObject *adjustment;
    int i;
    gint        argc,
	position = 0;
    gchar     **argv;
    guchar     *color_cube;
    gfloat vector[USER_CURVE_POINTS];

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

    build_preview_source_image();

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
    /*
    gtk_signal_connect(GTK_OBJECT(notebook), "switch_page",
		       GTK_SIGNAL_FUNC(dialog_page_switch), NULL);
    */
    gtk_notebook_set_tab_pos (GTK_NOTEBOOK (notebook), GTK_POS_TOP);
    gtk_box_pack_start(GTK_BOX(top_table), notebook, TRUE, TRUE, 0);
    gtk_widget_show(notebook);

	/* Settings */

	middle_table = gtk_table_new(2, 2, FALSE);
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
	    gtk_table_attach(GTK_TABLE(middle_table), alignment, 1, 2, 0, 2, GTK_FILL, 0, 0, 0);

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

	/* User Curve */

	user_curve = gtk_gamma_curve_new();
	gtk_widget_show(user_curve);

	label = gtk_label_new("Curve");
	gtk_widget_show(label);
	gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), user_curve, label, label);

	gtk_curve_set_range(GTK_CURVE(GTK_GAMMA_CURVE(user_curve)->curve),
			    0, USER_CURVE_POINTS - 1, 0, USER_CURVE_POINTS - 1);
	for (i = 0; i < USER_CURVE_POINTS; ++i)
	    vector[i] = mmvals.curve_points[i];
	gtk_curve_set_vector(GTK_CURVE(GTK_GAMMA_CURVE(user_curve)->curve),
			     USER_CURVE_POINTS, vector);
	gtk_curve_set_range(GTK_CURVE(GTK_GAMMA_CURVE(user_curve)->curve), 0, 1, 0, 1);

	/* Examples */

	table = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(table),
					GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_show (table);

	root_tree = read_tree_from_rc();
	gtk_signal_connect(GTK_OBJECT(root_tree), "selection_changed",
			   (GtkSignalFunc)dialog_tree_changed,
			   (gpointer)NULL);
#if 0
	gtk_container_add(GTK_CONTAINER(table), root_tree);
#else
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(table), root_tree);
#endif
	gtk_tree_set_selection_mode(GTK_TREE(root_tree), GTK_SELECTION_BROWSE);
	gtk_tree_set_view_lines(GTK_TREE(root_tree), FALSE);
	gtk_tree_set_view_mode(GTK_TREE(root_tree), FALSE);
	gtk_widget_show(root_tree);

	/*
	for (i = 0; examples[i][0] != 0; ++i)
	{
	    item2 = gtk_tree_item_new_with_label(examples[i][0]);
	    gtk_object_set_user_data(GTK_OBJECT(item2), examples[i][1]);
	    gtk_widget_show(item2);
	    gtk_tree_append(GTK_TREE(tree), item2);
	}
	*/

	label = gtk_label_new("Examples");
	gtk_widget_show(label);
	gtk_notebook_append_page_menu(GTK_NOTEBOOK(notebook), table, label, label);

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

    /* Done */

    gtk_widget_show(dialog);
    /*	dialog_update_preview(); */

    gtk_main();
    gdk_flush();

    if (the_tile != NULL) {
	gimp_tile_unref(the_tile, FALSE);
	the_tile = NULL;
    } /* if */

    g_free(wint.image);
    g_free(wint.wimage);

    return wint.run;
} /* mathmap_dialog */


/*****/

static void
dialog_update_preview(void)
{
    double  left, right, bottom, top;
    double  dx, dy;
    int     x, y;
    double  scale_x, scale_y;
    guchar *p_ul, *p_lr, *p;
    gint check,check_0,check_1; 

    update_curve();

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
    wholeImageWidth = img_width;
    wholeImageHeight = img_height;

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

    generate_code();

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
	    currentY = y * imageHeight / preview_height - middleY;
	    calc_ra();
	    update_pixel_internals();
	    result = EVAL_EXPR();
	    tuple_to_color(result, &redf, &greenf, &bluef, &alphaf);

	    p_ul[0] = redf * 255;
	    p_ul[1] = greenf * 255;
	    p_ul[2] = bluef * 255;

	    if (inputBPP == 2 || inputBPP == 4)
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
	update_curve();
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
    
} /* dialog_help_callback */

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
