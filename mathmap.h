/* -*- c -*- */

/*
 * mathmap.h
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

#ifndef __MATHMAP_H__
#define __MATHMAP_H__

#include <stdio.h>
#include <math.h>

#include "glib.h"
#include "exprtree.h"
#include "userval.h"
#include "internals.h"
#include "vars.h"
#include "compiler.h"
#include "color.h"

#include "lispreader/pools.h"

#ifndef OPENSTEP
#include <libgimp/gimp.h>
#endif

#define MATHMAP_DATE		"December 2007"

#define MAIN_TEMPLATE_FILENAME	"new_template.c"
#define OPMACROS_FILENAME	"opmacros.h"

#define IMAGE_FLAG_UNIT		0x0001
#define IMAGE_FLAG_SQUARE	0x0002

#define MATHMAP_FLAG_NATIVE	0x0100

typedef struct _interpreter_insn_t interpreter_insn_t;

typedef struct _filter_t
{
    int num_uservals;
    userval_info_t *userval_infos;

    internal_t *internals;
    variable_t *variables;

    top_level_decl_t *decl;

    struct _filter_t *next;
} filter_t;

typedef struct _mathmap_t
{
    filter_t *filters;
    filter_t *current_filter;	/* only valid during parsing */
    filter_t *main_filter;

    unsigned int flags;

    initfunc_t initfunc;
    void *module_info;

    interpreter_insn_t *interpreter_insns;
    // FIXME: This should be in the invocation!
    GArray *interpreter_values;

    struct _mathmap_t *next;
} mathmap_t;

/* If this is in the plug-in then 0, otherwise it's in the command
   line. */
extern int cmd_line_mode;

/* This variable is set by the compiler.  It's ok that it is global
   because the compiler is non-reentrant (which is ok because it's
   fast and more convenient to write). */
extern mathmap_t *the_mathmap;

/* This is incremented by the scanner for each line scanned. */
extern int scanner_line_num;

#ifndef OPENSTEP
extern color_t gradient_samples[USER_GRADIENT_POINTS];
#endif

#ifndef OPENSTEP
extern int fast_image_source_scale;
#endif

#define EDGE_BEHAVIOUR_COLOR          1	/* all four used in new_template.c */
#define EDGE_BEHAVIOUR_WRAP           2
#define EDGE_BEHAVIOUR_REFLECT        3
#define EDGE_BEHAVIOUR_ROTATE         4
#define EDGE_BEHAVIOUR_MASK	      0xff

#define EDGE_BEHAVIOUR_X_FLAG	      0x0100
#define EDGE_BEHAVIOUR_Y_FLAG	      0x0200

#define MAX_DEBUG_TUPLES              8

typedef struct _mathmap_invocation_t
{
    mathmap_t *mathmap;

    userval_t *uservals;

    int antialiasing;
    int supersampling;

    int output_bpp;

    int edge_behaviour_x, edge_behaviour_y;
    color_t edge_color_x, edge_color_y;

    int current_frame;

    /* These are in pixel coordinates: */
    int img_width, img_height;

    /* These are in virtual coordinates: */
    float middle_x, middle_y;
    float image_R, image_X, image_Y, image_W, image_H;

    /* The factors for converting between the two: */
    float scale_x, scale_y;

    float current_x, current_y, current_r, current_a, current_t;

    int row_stride;

    unsigned char * volatile rows_finished;

    mathfuncs_t mathfuncs;

    int do_debug;
    int num_debug_tuples;
    tuple_t *debug_tuples[MAX_DEBUG_TUPLES];

    int interpreter_ip;
    color_t interpreter_output_color;
} mathmap_invocation_t;

typedef struct _mathmap_slice_t
{
    mathmap_invocation_t *invocation;

    float sampling_offset_x, sampling_offset_y;
    int region_x, region_y, region_width, region_height;

    void *xy_vars;
    void *y_vars;

    pools_t pools;
} mathmap_slice_t;

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

void calc_scale_factors (unsigned int flags, int pixel_width, int pixel_height, float *scale_x, float *scale_y);
void calc_middle_values (int img_width, int img_height, float scale_x, float scale_y, float *middle_x, float *middle_y);

#ifdef MATHMAP_CMDLINE
int cmdline_main (int argc, char *argv[]);
color_t cmdline_mathmap_get_pixel (mathmap_invocation_t *invocation, input_drawable_t *drawable, int frame, int x, int y);
#endif

void register_args_as_uservals (filter_t *filter, arg_decl_t *arg_decls);

void unload_mathmap (mathmap_t *mathmap);
void free_mathmap (mathmap_t *mathmap);
void free_invocation (mathmap_invocation_t *invocation);

void enable_debugging (mathmap_invocation_t *invocation);
void disable_debugging (mathmap_invocation_t *invocation);

int does_filter_use_ra (filter_t *filter);
int does_filter_use_t (filter_t *filter);

void start_parsing_filter (mathmap_t *mathmap, top_level_decl_t *decl);
void finish_parsing_filter (mathmap_t *mathmap);

int check_mathmap (char *expression);
mathmap_t* parse_mathmap (char *expression);
mathmap_t* compile_mathmap (char *expression, char *template_filename, char *include_path);
mathmap_invocation_t* invoke_mathmap (mathmap_t *mathmap, mathmap_invocation_t *template, int img_width, int img_height);
void init_frame (mathmap_slice_t *slice);

gpointer call_invocation_parallel (mathmap_invocation_t *invocation,
				   int region_x, int region_y, int region_width, int region_height,
				   unsigned char *q, int num_threads);
void call_invocation_parallel_and_join (mathmap_invocation_t *invocation,
					int region_x, int region_y, int region_width, int region_height,
					unsigned char *q, int num_threads);

void join_invocation_call (gpointer *_call);
void kill_invocation_call (gpointer *_call);
gboolean invocation_call_is_done (gpointer *_call);

void carry_over_uservals_from_template (mathmap_invocation_t *invocation, mathmap_invocation_t *template);

void update_image_internals (mathmap_invocation_t *invocation);

color_t mathmap_get_pixel (mathmap_invocation_t *invocation, input_drawable_t *drawable, int frame, int x, int y);

typedef int (*template_processor_func_t) (mathmap_t *mathmap, const char *directive, const char *arg, FILE *out, void *data);

void drawable_get_pixel_inc (mathmap_invocation_t *invocation, input_drawable_t *drawable, int *inc_x, int *inc_y);

void process_template (mathmap_t *mathmap, const char *template,
		       FILE *out, template_processor_func_t template_processor, void *user_data);
gboolean process_template_file (mathmap_t *mathmap, char *template_filename,
				FILE *out, template_processor_func_t template_processor, void *user_data);
int generate_plug_in (char *filter, char *output_filename,
		      char *template_filename, int analyze_constants,
		      template_processor_func_t template_processor);

void user_value_changed (void);

void delete_expression_marker (void);
void set_expression_marker (int line, int column);

int get_num_cpus (void);

#ifndef OPENSTEP
#define GIMP_DRAWABLE_ID(d)     ((d)->drawable_id)
#endif

#ifdef USE_PTHREADS
#include <pthread.h>
typedef pthread_t thread_handle_t;
#else
typedef gpointer thread_handle_t;
#endif

thread_handle_t mathmap_thread_start (void (*func) (gpointer), gpointer data);
void mathmap_thread_join (thread_handle_t thread);
void mathmap_thread_kill (thread_handle_t thread);

#define CALLBACK_SYMBOL __attribute((visibility("default")))

#endif
