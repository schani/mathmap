/* -*- c -*- */

/*
 * mathmap.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2009 Mark Probst
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

#ifndef __MATHMAP_H__
#define __MATHMAP_H__

#include <stdio.h>
#include <math.h>
#include <libintl.h>

#include "glib.h"
#include "exprtree.h"
#include "userval.h"
#include "internals.h"
#include "vars.h"
#include "compiler.h"
#include "color.h"
#include "expression_db.h"
#include "mmpools.h"

#define MATHMAP_DATE		"August 2009"

#ifdef USE_LLVM
#define MAIN_TEMPLATE_FILENAME  "llvm_template.o"
#else
#define MAIN_TEMPLATE_FILENAME	"new_template.c"
#define OPMACROS_FILENAME	"opmacros.h"
#endif

#define IMAGE_FLAG_UNIT		0x0001 /* unit coordinate system (vs. pixel) */
#define IMAGE_FLAG_SQUARE	0x0002 /* square pixels */

#define FILTER_MATHMAP		1
#define FILTER_NATIVE		2

struct _mathmap_invocation_t;

typedef image_t* (*native_filter_func_t) (struct _mathmap_invocation_t *invocation, userval_t *args,
					  mathmap_pools_t *pools);

typedef struct _filter_t
{
    int kind;

    char *name;

    int num_uservals;
    userval_info_t *userval_infos;

    union
    {
	struct
	{
	    internal_t *internals;
	    variable_t *variables;

	    top_level_decl_t *decl;
	} mathmap;
	struct
	{
	    gboolean needs_rendered_images;
	    gboolean is_pure;
	    char *func_name;
	    native_filter_func_t func; /* we only store this for easy lookup for caching */
	} native;
    } v;

    struct _filter_t *next;
} filter_t;

/* TEMPLATE mathmap */
typedef struct _mathmap_t
{
    filter_t *filters;
    filter_t *current_filter;	/* only valid during parsing */
    filter_t *main_filter;

    unsigned int flags;

    /* for CC */
    initfunc_t initfunc;
    /* FIXME: for LLVM - remove eventually */
    struct _mathfuncs_t *mathfuncs;

    void *module_info;

    struct _mathmap_t *next;
} mathmap_t;
/* END */

/* If this is in the plug-in then 0, otherwise it's in the command
   line. */
extern int cmd_line_mode;

/* This variable is set by the compiler.  It's ok that it is global
   because the compiler is non-reentrant (which is ok because it's
   fast and more convenient to write). */
extern mathmap_t *the_mathmap;

#ifndef OPENSTEP
extern color_t gradient_samples[USER_GRADIENT_POINTS];
#endif

#ifndef OPENSTEP
extern int fast_image_source_scale;
#endif

/* TEMPLATE edge_behaviour */
#define EDGE_BEHAVIOUR_COLOR          1
#define EDGE_BEHAVIOUR_WRAP           2
#define EDGE_BEHAVIOUR_REFLECT        3
#define EDGE_BEHAVIOUR_ROTATE         4
/* END */
#define EDGE_BEHAVIOUR_MASK	      0xff

#define EDGE_BEHAVIOUR_X_FLAG	      0x0100
#define EDGE_BEHAVIOUR_Y_FLAG	      0x0200

/* TEMPLATE max_debug_tuples */
#define MAX_DEBUG_TUPLES              8
/* END */

/* TEMPLATE orig_val_pixel_func */
typedef color_t (*orig_val_pixel_func_t) (struct _mathmap_invocation_t*, float, float, image_t*, int);
/* END */

typedef struct _native_filter_cache_entry_t
{
    filter_t *filter;
    userval_t *args;
    image_t *image;		/* NULL if not done */
    struct _native_filter_cache_entry_t *next;
} native_filter_cache_entry_t;

/* TEMPLATE invocation_frame_slice */
typedef struct _mathmap_invocation_t
{
    mathmap_t *mathmap;

    userval_t *uservals;

    /* FIXME: These should eventually go into image_t */
    int antialiasing;
    orig_val_pixel_func_t orig_val_func;

    int supersampling;

    int output_bpp;

    int edge_behaviour_x, edge_behaviour_y;
    color_t edge_color_x, edge_color_y;

    /* These are in pixel coordinates: */
    int img_width, img_height;
    int render_width, render_height;

    /* These are in virtual coordinates: */
    float image_R;		/* FIXME: remove and calculate in
				   filter code */

    int row_stride;

    unsigned char * volatile rows_finished;

    mathmap_pools_t pools;	/* used exclusively for the native filter cache */
    GMutex *native_filter_cache_mutex;
    GCond *native_filter_cache_cond;
    native_filter_cache_entry_t *native_filter_cache;

    /* FIXME: remove - it's in the closure */
    mathfuncs_t mathfuncs;

    int do_debug;
    int num_debug_tuples;
    tuple_t *debug_tuples[MAX_DEBUG_TUPLES];
} mathmap_invocation_t;

typedef struct _mathmap_frame_t
{
    mathmap_invocation_t *invocation;

    int frame_render_width, frame_render_height;

    int current_frame;
    float current_t;

    void *xy_vars;
    mathmap_pools_t pools;
} mathmap_frame_t;

typedef struct _mathmap_slice_t
{
    mathmap_frame_t *frame;

    float sampling_offset_x, sampling_offset_y;
    int region_x, region_y, region_width, region_height;

    void *y_vars;
    mathmap_pools_t pools;
} mathmap_slice_t;
/* END */

typedef struct
{
    int num_debug_tuples;
    tuple_t *debug_tuples[MAX_DEBUG_TUPLES];
} pixel_debug_info_t;

#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

#ifndef M_PI
#define M_PI     3.14159265358979323846
#endif

/* TEMPLATE llvm_mathfuncs */
void llvm_filter_init_frame (mathmap_frame_t *mmframe, image_t *closure);
void llvm_filter_init_slice (mathmap_slice_t *slice, image_t *closure);
void llvm_filter_calc_lines (mathmap_slice_t *slice, image_t *closure, int first_row, int last_row, void *q, int floatmap);
/* END */


int cmdline_main (int argc, char *argv[]);
color_t cmdline_mathmap_get_pixel (mathmap_invocation_t *invocation, input_drawable_t *drawable, int frame, int x, int y);

userval_info_t* arg_decls_to_uservals (filter_t *filter, arg_decl_t *arg_decls);
void register_args_as_uservals (filter_t *filter, arg_decl_t *arg_decls);

void unload_mathmap (mathmap_t *mathmap);
void free_mathmap (mathmap_t *mathmap);
void free_invocation (mathmap_invocation_t *invocation);

void enable_debugging (mathmap_invocation_t *invocation);
void disable_debugging (mathmap_invocation_t *invocation);

int does_filter_use_ra (filter_t *filter);
int does_filter_use_t (filter_t *filter);

unsigned int filter_flags (filter_t *filter);

void start_parsing_filter (mathmap_t *mathmap, top_level_decl_t *decl);
void finish_parsing_filter (mathmap_t *mathmap);

int check_mathmap (char *expression);
mathmap_t* parse_mathmap (char *expression);
mathmap_t* compile_mathmap (char *expression, char **support_paths, int timeout, gboolean no_backend);
mathmap_invocation_t* invoke_mathmap (mathmap_t *mathmap, mathmap_invocation_t *template_invocation,
				      int img_width, int img_height, gboolean copy_first_image);

mathmap_frame_t* invocation_new_frame (mathmap_invocation_t *invocation, image_t *closure,
				       int current_frame, float current_t);
void invocation_free_frame (mathmap_frame_t *frame);

void invocation_init_slice (mathmap_slice_t *slice, image_t *image, mathmap_frame_t *frame, int region_x, int region_y,
			    int region_width, int region_height, float sampling_offset_x, float sampling_offset_y);
void invocation_deinit_slice (mathmap_slice_t *slice);

void invocation_set_antialiasing (mathmap_invocation_t *invocation, gboolean antialising);

gpointer call_invocation_parallel (mathmap_frame_t *frame, image_t *closure,
				   int region_x, int region_y, int region_width, int region_height,
				   unsigned char *q, int num_threads);
void call_invocation_parallel_and_join (mathmap_frame_t *frame, image_t *closure,
					int region_x, int region_y, int region_width, int region_height,
					unsigned char *q, int num_threads);

void join_invocation_call (gpointer *_call);
void kill_invocation_call (gpointer *_call);
gboolean invocation_call_is_done (gpointer *_call);

native_filter_cache_entry_t* invocation_lookup_native_filter_invocation (mathmap_invocation_t *invocation, userval_t *args,
									 native_filter_func_t filter_func);
void native_filter_cache_entry_set_image (mathmap_invocation_t *invocation,
					  native_filter_cache_entry_t *cache_entry,
					  image_t *image);

void carry_over_uservals_from_template (mathmap_invocation_t *invocation, mathmap_invocation_t *template_invocation,
					gboolean copy_first_image);

color_t mathmap_get_pixel (mathmap_invocation_t *invocation, input_drawable_t *drawable, int frame, int x, int y);

typedef int (*template_processor_func_t) (mathmap_t *mathmap, const char *directive, const char *arg, FILE *out, void *data);

void drawable_get_pixel_inc (mathmap_invocation_t *invocation, input_drawable_t *drawable, int *inc_x, int *inc_y);

void process_template (mathmap_t *mathmap, const char *template_filename,
		       FILE *out, template_processor_func_t template_processor, void *user_data);
gboolean process_template_file (mathmap_t *mathmap, char *template_filename,
				FILE *out, template_processor_func_t template_processor, void *user_data);
int generate_plug_in (char *filter, char *output_filename,
		      char *template_filename, int analyze_constants,
		      template_processor_func_t template_processor);

void user_value_changed (void);

void delete_expression_marker (void);
void set_expression_marker (int start_line, int start_column, int end_line, int end_column);

int get_num_cpus (void);

designer_design_type_t* design_type_from_expression_db (expression_db_t **edb);

#ifndef OPENSTEP
#define GIMP_DRAWABLE_ID(d)     ((d)->drawable_id)
#endif

#if defined(USE_PTHREADS)
#include <pthread.h>
typedef pthread_t thread_handle_t;
#elif defined(USE_GTHREADS)
typedef GThread* thread_handle_t;
#else
typedef gpointer thread_handle_t;
#endif

thread_handle_t mathmap_thread_start (void (*func) (gpointer), gpointer data);
void mathmap_thread_join (thread_handle_t thread);
void mathmap_thread_kill (thread_handle_t thread);

char* make_filter_source_from_design (designer_design_t *design, const char *filter_name);

void mathmap_message_dialog (const char *message);

void init_gettext ();

#define _(x)	gettext((x))

#define CALLBACK_SYMBOL __attribute((visibility("default")))

#endif
