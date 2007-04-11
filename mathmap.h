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

#include "gtypes.h"
#include "exprtree.h"
#include "userval.h"
#include "internals.h"
#include "vars.h"
#include "compiler.h"
#include "color.h"

#include <libgimp/gimp.h>

#define MATHMAP_DATE          "April 2007"

#define MAIN_TEMPLATE_FILENAME	"new_template.c"
#define OPMACROS_FILENAME	"opmacros.h"

#define IMAGE_FLAG_UNIT		0x0001
#define IMAGE_FLAG_SQUARE	0x0002

#define MATHMAP_FLAG_NATIVE	0x0100

typedef struct _interpreter_insn_t interpreter_insn_t;

typedef struct _mathmap_t
{
    userval_info_t *userval_infos;
    variable_t *variables;
    internal_t *internals;

    //exprtree *exprtree;
    top_level_decl_t *top_level_decls;

    int num_uservals;

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

extern color_t gradient_samples[USER_GRADIENT_POINTS];

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
    tuple_t **variables;

    int antialiasing;
    int supersampling;

    int output_bpp;

    int edge_behaviour_x, edge_behaviour_y;
    color_t edge_color_x, edge_color_y;

    int current_frame;

    /* These are in pixel coordinates: */
    int origin_x, origin_y;
    int img_width, img_height;
    int calc_img_width, calc_img_height;
    float sampling_offset_x, sampling_offset_y;

    /* These are in virtual coordinates: */
    float middle_x, middle_y;
    float image_R, image_X, image_Y, image_W, image_H;

    /* The factors for converting between the two: */
    float scale_x, scale_y;

    float current_x, current_y, current_r, current_a, current_t;

    int row_stride;
    volatile int num_rows_finished;

    mathfuncs_t mathfuncs;

    void *xy_vars;
    void *y_vars;

    int do_debug;
    int num_debug_tuples;
    tuple_t *debug_tuples[MAX_DEBUG_TUPLES];

    int interpreter_ip;
    color_t interpreter_output_color;
} mathmap_invocation_t;

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

// fprintf with the C locale
int fprintf_c (FILE *stream, const char *format, ...);

void calc_scale_factors (unsigned int flags, int pixel_width, int pixel_height, float *scale_x, float *scale_y);
void calc_middle_values (int img_width, int img_height, float scale_x, float scale_y, float *middle_x, float *middle_y);

#ifdef MATHMAP_CMDLINE
int cmdline_main (int argc, char *argv[]);
color_t cmdline_mathmap_get_pixel (mathmap_invocation_t *invocation, userval_t *userval, int frame, int x, int y);
#endif

void register_args_as_uservals (mathmap_t *mathmap, arg_decl_t *arg_decls);

void unload_mathmap (mathmap_t *mathmap);
void free_mathmap (mathmap_t *mathmap);
void free_invocation (mathmap_invocation_t *invocation);

void enable_debugging (mathmap_invocation_t *invocation);
void disable_debugging (mathmap_invocation_t *invocation);

int does_mathmap_use_ra (mathmap_t *mathmap);
int does_mathmap_use_t (mathmap_t *mathmap);

int check_mathmap (char *expression);
mathmap_t* parse_mathmap (char *expression);
mathmap_t* compile_mathmap (char *expression, FILE *template, char *opmacros_filename);
mathmap_invocation_t* invoke_mathmap (mathmap_t *mathmap, mathmap_invocation_t *template, int img_width, int img_height);
void init_frame (mathmap_invocation_t *invocation);
void call_invocation (mathmap_invocation_t *invocation, int first, int last, unsigned char *p);

void carry_over_uservals_from_template (mathmap_invocation_t *invocation, mathmap_invocation_t *template);

void update_image_internals (mathmap_invocation_t *invocation);

color_t mathmap_get_pixel (mathmap_invocation_t *invocation, userval_t *userval, int frame, int x, int y);
color_t mathmap_get_fast_pixel (mathmap_invocation_t *invocation, userval_t *userval, int x, int y);

typedef int (*template_processor_func_t) (mathmap_t *mathmap, const char *directive, FILE *out);

void process_template_file (mathmap_t *mathmap, FILE *template, FILE *out, template_processor_func_t template_processor);
int generate_plug_in (char *filter, char *output_filename,
		      char *template_filename, char *opmacros_filename, int analyze_constants,
		      template_processor_func_t template_processor);

void user_value_changed (void);

void set_expression_cursor (int line, int column);

int alloc_input_drawable (GimpDrawable *drawable);
void free_input_drawable (int index);
GimpDrawable* get_input_drawable (int index);

#define DRAWABLE_ID(d)     ((d)->drawable_id)

#endif
