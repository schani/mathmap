/* -*- c -*- */

/*
 * mathmap_common.c
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

#include <math.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#ifdef USE_PTHREADS
#include <pthread.h>
#endif
#include <locale.h>

#include "internals.h"
#include "tags.h"
#include "jump.h"
#include "scanner.h"
#include "compiler.h"
#include "mathmap.h"

int cmd_line_mode = 0;

mathmap_t *the_mathmap = 0;
int scanner_line_num;

gboolean report_parse_error_to_user = FALSE;

/* from parser.y */
int yyparse (void);

static unsigned int
image_flags_from_options (option_t *options)
{
    unsigned int flags = 0;

    if (find_option_with_name(options, "pixel") == NULL)
    {
	flags |= IMAGE_FLAG_UNIT;
	if (find_option_with_name(options, "stretched") == NULL)
	    flags |= IMAGE_FLAG_SQUARE;
    }

    return flags;
}

unsigned int
filter_flags (filter_t *filter)
{
    g_assert(filter->kind == FILTER_MATHMAP);
    return image_flags_from_options(filter->v.mathmap.decl->v.filter.options);
}

userval_info_t*
arg_decls_to_uservals (filter_t *filter, arg_decl_t *arg_decls)
{
    userval_info_t *infos = NULL;

    while (arg_decls != 0)
    {
	userval_info_t *result = 0;

	if (lookup_userval(infos, arg_decls->name) != NULL)
	{
	    sprintf(error_string, _("The argument `%s' is declared more than once."), arg_decls->name);
	    JUMP(1);
	}

	switch (arg_decls->type)
	{
	    case ARG_TYPE_INT :
		if (arg_decls->v.integer.have_limits)
		    result = register_int_const(&infos, arg_decls->name,
						arg_decls->v.integer.min, arg_decls->v.integer.max,
						arg_decls->v.integer.default_value);
		else
		    result = register_int_const(&infos, arg_decls->name, -100000, 100000, 0);
		break;

	    case ARG_TYPE_FLOAT :
		if (arg_decls->v.floating.have_limits)
		    result = register_float_const(&infos, arg_decls->name,
						  arg_decls->v.floating.min, arg_decls->v.floating.max,
						  arg_decls->v.floating.default_value);
		else
		    result = register_float_const(&infos, arg_decls->name, -1.0, 1.0, 0.0);
		break;

	    case ARG_TYPE_BOOL :
		result = register_bool(&infos, arg_decls->name, arg_decls->v.boolean.default_value);
		break;

	    case ARG_TYPE_COLOR :
		result = register_color(&infos, arg_decls->name);
		break;

	    case ARG_TYPE_GRADIENT :
		result = register_gradient(&infos, arg_decls->name);
		break;

	    case ARG_TYPE_CURVE :
		result = register_curve(&infos, arg_decls->name);
		break;

	    case ARG_TYPE_FILTER :
		assert(0);

	    case ARG_TYPE_IMAGE :
		result = register_image(&infos, arg_decls->name,
					image_flags_from_options(arg_decls->options));
		break;

	    default :
		assert(0);
	}

	if (result == 0)
	{
	    sprintf(error_string, _("Conflict for argument %s."), arg_decls->name);
	    JUMP(1);
	}

	arg_decls = arg_decls->next;
    }

    return infos;
}

static int
count_userval_infos (userval_info_t *info)
{
    int count = 0;

    while (info != NULL)
    {
	++count;
	info = info->next;
    }

    return count;
}

void
register_args_as_uservals (filter_t *filter, arg_decl_t *arg_decls)
{
    arg_decl_t *decl;

    for (decl = arg_decls; decl != NULL; decl = decl->next)
	if (lookup_internal(filter->v.mathmap.internals, decl->name, TRUE) != NULL
	    || lookup_variable_macro(decl->name, NULL) != NULL)
	{
	    sprintf(error_string, _("Argument `%s' has the same name as an internal variable."), decl->name);
	    JUMP(1);
	}

    g_assert(filter->userval_infos == NULL && filter->num_uservals == 0);

    filter->userval_infos = arg_decls_to_uservals(filter, arg_decls);
    filter->num_uservals = count_userval_infos(filter->userval_infos);
}

static void
init_internals (filter_t *filter)
{
    g_assert(filter->kind == FILTER_MATHMAP);

    register_internal(&filter->v.mathmap.internals, "x", CONST_Y | CONST_T);
    register_internal(&filter->v.mathmap.internals, "y", CONST_X | CONST_T);
    register_internal(&filter->v.mathmap.internals, "r", CONST_T);
    register_internal(&filter->v.mathmap.internals, "a", CONST_T);
    register_internal(&filter->v.mathmap.internals, "t", CONST_X | CONST_Y);
    register_internal(&filter->v.mathmap.internals, "R", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->v.mathmap.internals, "__canvasPixelW", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->v.mathmap.internals, "__canvasPixelH", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->v.mathmap.internals, "__renderPixelW", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->v.mathmap.internals, "__renderPixelH", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->v.mathmap.internals, "frame", CONST_X | CONST_Y);

    /* These are resolved by the compiler as bindings, i.e. calculated
       in the filter code */
    register_internal(&filter->v.mathmap.internals, "X", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->v.mathmap.internals, "Y", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->v.mathmap.internals, "W", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->v.mathmap.internals, "H", CONST_X | CONST_Y | CONST_T);
}

void
start_parsing_filter (mathmap_t *mathmap, top_level_decl_t *decl)
{
    filter_t *filter;

    g_assert(mathmap->current_filter == NULL);

    filter = g_new0(filter_t, 1);

    filter->kind = FILTER_MATHMAP;
    filter->name = g_strdup(decl->name);

    filter->v.mathmap.decl = decl;

    init_internals(filter);

    filter->next = mathmap->filters;
    mathmap->filters = filter;

    mathmap->current_filter = filter;
}

void
finish_parsing_filter (mathmap_t *mathmap)
{
    filter_t *filter = mathmap->current_filter;

    g_assert(filter != NULL);

    mathmap->current_filter = 0;
}

static void
free_filters (filter_t *filter)
{
    while (filter != 0)
    {
	filter_t *next = filter->next;

	g_free(filter->name);

	if (filter->userval_infos != NULL)
	    free_userval_infos(filter->userval_infos);

	if (filter->kind == FILTER_MATHMAP && filter->v.mathmap.variables != NULL)
	    free_variables(filter->v.mathmap.variables);
	else if (filter->kind == FILTER_NATIVE)
	    g_free(filter->v.native.func_name);

	g_free(filter);

	filter = next;
    }
}

void
unload_mathmap (mathmap_t *mathmap)
{
    if (mathmap->module_info != 0)
    {
#ifdef USE_LLVM
	unload_llvm_code(mathmap->module_info);
#else
	unload_c_code(mathmap->module_info);
#endif
	mathmap->module_info = 0;
    }
}

void
free_mathmap (mathmap_t *mathmap)
{
    if (mathmap->filters != 0)
	free_filters(mathmap->filters);
    unload_mathmap(mathmap);

    free(mathmap);
}

void
free_invocation (mathmap_invocation_t *invocation)
{
    if (invocation->uservals != 0)
    {
	free_uservals(invocation->uservals, invocation->mathmap->main_filter->userval_infos);
	free(invocation->uservals);
    }

    free(invocation->rows_finished);
    free(invocation);
}

static filter_t*
register_native_filter (mathmap_t *mathmap, const char *name, userval_info_t *userval_infos,
			gboolean needs_rendered_images, gboolean is_pure, const char *filter_func_name)
{
    filter_t *filter = g_new0(filter_t, 1);

    filter->kind = FILTER_NATIVE;
    filter->name = g_strdup(name);

    filter->userval_infos = userval_infos;
    filter->num_uservals = count_userval_infos(userval_infos);

    filter->v.native.needs_rendered_images = needs_rendered_images;
    filter->v.native.is_pure = is_pure;
    filter->v.native.func_name = g_strdup(filter_func_name);

    filter->next = mathmap->filters;
    mathmap->filters = filter;

    return NULL;
}

static void
register_native_filters (mathmap_t *mathmap)
{
    userval_info_t *infos;

    infos = NULL;
    register_image(&infos, "in", 0);
    register_float_const(&infos, "horizontal_std_dev", 0.0, 2.0, 0.01);
    register_float_const(&infos, "vertical_std_dev", 0.0, 2.0, 0.01);
    register_native_filter(mathmap, "gaussian_blur", infos, TRUE, TRUE, "native_filter_gaussian_blur");

    infos = NULL;
    register_image(&infos, "in", 0);
    register_image(&infos, "kernel", 0);
    register_bool(&infos, "normalize", 1.0);
    register_bool(&infos, "copy_alpha", 1.0);
    register_native_filter(mathmap, "convolve", infos, TRUE, TRUE, "native_filter_convolve");

    infos = NULL;
    register_image(&infos, "in", 0);
    register_image(&infos, "mask", 0);
    register_bool(&infos, "copy_alpha", 1.0);
    register_native_filter(mathmap, "half_convolve", infos, TRUE, TRUE, "native_filter_half_convolve");

    infos = NULL;
    register_image(&infos, "in", 0);
    register_bool(&infos, "ignore_alpha", 1.0);
    register_native_filter(mathmap, "visualize_fft", infos, TRUE, TRUE, "native_filter_visualize_fft");
}

#define X_INTERNAL_INDEX         0
#define Y_INTERNAL_INDEX         1
#define R_INTERNAL_INDEX         2
#define A_INTERNAL_INDEX         3

int
does_filter_use_ra (filter_t *filter)
{
    internal_t *r_internal, *a_internal;

    g_assert(filter->kind == FILTER_MATHMAP);

    r_internal = lookup_internal(filter->v.mathmap.internals, "r", 1);
    a_internal = lookup_internal(filter->v.mathmap.internals, "a", 1);
    g_assert(r_internal != NULL && a_internal != NULL);

    return r_internal->is_used || a_internal->is_used;
}

int
does_filter_use_t (filter_t *filter)
{
    internal_t *t_internal;

    g_assert(filter->kind == FILTER_MATHMAP);

    t_internal = lookup_internal(filter->v.mathmap.internals, "t", 1);
    g_assert(t_internal != NULL);

    return t_internal->is_used;
}

mathmap_t*
parse_mathmap (char *expression, gboolean report_error)
{
    static mathmap_t *mathmap;	/* this is static to avoid problems with longjmp.  */
    filter_t *filter;

    mathmap = g_new0(mathmap_t, 1);

    the_mathmap = mathmap;

    register_native_filters(mathmap);

    DO_JUMP_CODE {
	scanner_line_num = 0;
	scanFromString(expression);
	report_parse_error_to_user = report_error;
	yyparse();
	endScanningFromString();

	if (mathmap->filters == NULL || mathmap->filters->kind != FILTER_MATHMAP)
	{
	    free_filters(mathmap->filters);
	    mathmap->filters = 0;

	    sprintf(error_string, _("At least one filter must be defined."));
	    JUMP(1);
	}

	for (filter = mathmap->filters; filter != 0; filter = filter->next)
	{
	    exprtree *expr;

	    if (filter->kind != FILTER_MATHMAP)
		continue;

	    if (filter->v.mathmap.decl->type != TOP_LEVEL_FILTER)
	    {
		free_filters(mathmap->filters);
		mathmap->filters = 0;

		sprintf(error_string, _("Top-level declarations can only be filters."));
		JUMP(1);
	    }

	    expr = filter->v.mathmap.decl->v.filter.body;

	    if (expr->result.number != rgba_tag_number
		|| expr->result.length != 4)
	    {
		sprintf(error_string, _("The filter `%s' must have the result type rgba:4."), filter->name);

		free_filters(mathmap->filters);
		mathmap->filters = 0;

		JUMP(1);
	    }
	}

	mathmap->main_filter = mathmap->filters;

	mathmap->flags = 0;
    } WITH_JUMP_HANDLER {
	free_mathmap(mathmap);
	mathmap = 0;
    } END_JUMP_HANDLER;

    the_mathmap = 0;

    return mathmap;
}

int
check_mathmap (char *expression)
{
    mathmap_t *mathmap = parse_mathmap(expression, TRUE);

    if (mathmap != 0)
    {
	free_mathmap(mathmap);
	return 1;
    }
    else
	return 0;
}

mathmap_t*
compile_mathmap (char *expression, char *template_filename, char *include_path)
{
    static mathmap_t *mathmap;	/* this is static to avoid problems with longjmp.  */

    DO_JUMP_CODE {
	mathmap = parse_mathmap(expression, TRUE);

	if (mathmap == 0)
	{
	    JUMP(1);
	}

#ifdef USE_LLVM
	gen_and_load_llvm_code(mathmap, template_filename);
#else
	mathmap->initfunc = gen_and_load_c_code(mathmap, &mathmap->module_info, template_filename, include_path);
#endif
	if (mathmap->initfunc == 0 && mathmap->filter_func == 0)
	{
	    char *message = g_strdup_printf(_("The MathMap compiler failed.  Since this development\n"
					      "release does not provide a fallback interpreter that\n"
					      "means that MathMap won't work.\n"
					      "This is the reason why the compiler failed:\n%s"), error_string);

	    strcpy(error_string, message);

	    g_free(message);

	    JUMP(1);
	}

	delete_expression_marker();
    } WITH_JUMP_HANDLER {
	if (mathmap != 0)
	{
	    free_mathmap(mathmap);
	    mathmap = 0;
	}
    } END_JUMP_HANDLER;

    return mathmap;
}

void
llvm_filter_init_frame (mathmap_frame_t *mmframe, image_t *closure)
{
    mathmap_invocation_t *invocation = mmframe->invocation;

#ifdef POOLS_DEBUG_OUTPUT
    printf("initing frame %p (pools %p)\n", mmframe, &mmframe->pools);
#endif

    mmframe->xy_vars = invocation->mathmap->llvm_init_frame_func(invocation, closure, mmframe->current_t, &mmframe->pools);
}

void
llvm_filter_init_slice (mathmap_slice_t *slice, image_t *closure)
{
    mathmap_frame_t *mmframe = slice->frame;
    mathmap_invocation_t *invocation = mmframe->invocation;
    float t = mmframe->current_t;
    pools_t *pools = &slice->pools;
    int col;

#ifdef POOLS_DEBUG_OUTPUT
    printf("initing slice %p (pools %p)\n", slice, pools);
#endif

    slice->y_vars = pools_alloc(pools, sizeof(void*) * slice->region_width);

    for (col = 0; col < slice->region_width; ++col)
    {
	float x = CALC_VIRTUAL_X(col + slice->region_x, mmframe->frame_render_width, slice->sampling_offset_x);
	void *y_vars = invocation->mathmap->init_y_func(slice, closure, x, t);

	((void**)slice->y_vars)[col] = y_vars;
    }
}

void
llvm_filter_calc_lines (mathmap_slice_t *slice, image_t *closure, int first_row, int last_row, void *q, int floatmap)
{
    mathmap_frame_t *mmframe = slice->frame;
    mathmap_invocation_t *invocation = mmframe->invocation;
    int row, col;
    float t = mmframe->current_t;
    float sampling_offset_x = slice->sampling_offset_x, sampling_offset_y = slice->sampling_offset_y;
    int output_bpp = invocation->output_bpp;
    int is_bw = output_bpp == 1 || output_bpp == 2;
    int need_alpha = output_bpp == 2 || output_bpp == 4;
    int alpha_index = output_bpp - 1;
    pools_t pixel_pools;
    pools_t *pools;
    int region_x = slice->region_x;
    int frame_render_width = mmframe->frame_render_width;
    int frame_render_height = mmframe->frame_render_height;

    init_pools(&pixel_pools);
    pools = &pixel_pools;

#ifdef POOLS_DEBUG_OUTPUT
    printf("calcing lines in slice %p with pools %p\n", slice, pools);
#endif

    first_row = MAX(0, first_row);
    last_row = MIN(last_row, slice->region_y + slice->region_height);

    for (row = first_row - slice->region_y; row < last_row - slice->region_y; ++row)
    {
	float y = CALC_VIRTUAL_Y(row + slice->region_y, frame_render_height, sampling_offset_y);
	unsigned char *p = q;
	float *fp = q;
	void *x_vars;

#ifdef POOLS_DEBUG_OUTPUT
	printf("calcing x_vars for row %d\n", row);
#endif
	x_vars = invocation->mathmap->init_x_func(slice, closure, y, t);

	for (col = 0; col < slice->region_width; ++col)
	{
	    void *y_vars = ((void**)slice->y_vars)[col];
	    float x = CALC_VIRTUAL_X(col + region_x, frame_render_width, sampling_offset_x);
	    float *return_tuple;

	    reset_pools(pools);

#ifdef POOLS_DEBUG_OUTPUT
	    printf("calcing row %d col %d\n", row, col);
#endif
	    return_tuple = invocation->mathmap->main_filter_func(slice, closure, x_vars, y_vars, x, y, t, pools);
#ifdef POOLS_DEBUG_OUTPUT
	    printf("got return tuple %p\n", return_tuple);
#endif

	    if (floatmap)
	    {
		int i;

		for (i = 0; i < NUM_FLOATMAP_CHANNELS; ++i)
		    fp[i] = return_tuple[i];
	    }
	    else
	    {
		if (is_bw)
		    p[0] = (TUPLE_RED(return_tuple) * 0.299
			    + TUPLE_GREEN(return_tuple) * 0.587
			    + TUPLE_BLUE(return_tuple) * 0.114) * 255.0;
		else
		{
		    p[0] = TUPLE_RED(return_tuple) * 255.0;
		    p[1] = TUPLE_GREEN(return_tuple) * 255.0;
		    p[2] = TUPLE_BLUE(return_tuple) * 255.0;
		}
		if (need_alpha)
		    p[alpha_index] = TUPLE_ALPHA(return_tuple) * 255.0;
	    }

	    p += output_bpp;
	    fp += NUM_FLOATMAP_CHANNELS;
	}

	if (floatmap)
	    q = (float*)q + frame_render_width * NUM_FLOATMAP_CHANNELS;
	else
	    q = (unsigned char*)q + invocation->row_stride;

	if (!invocation->supersampling)
	    invocation->rows_finished[row] = 1;
    }

    free_pools(&pixel_pools);
}

static void
init_invocation (mathmap_invocation_t *invocation)
{
    if (invocation->mathmap->initfunc)
	invocation->mathfuncs = invocation->mathmap->initfunc(invocation);
    else
    {
	assert(invocation->mathmap->filter_func);
	invocation->mathfuncs.init_frame = llvm_filter_init_frame;
	invocation->mathfuncs.init_slice = llvm_filter_init_slice;
	invocation->mathfuncs.calc_lines = llvm_filter_calc_lines;
    }
}

void
invocation_set_antialiasing (mathmap_invocation_t *invocation, gboolean antialiasing)
{
    invocation->antialiasing = antialiasing;
    if (antialiasing)
	invocation->orig_val_func = get_orig_val_intersample_pixel;
    else
	invocation->orig_val_func = get_orig_val_pixel;
}

mathmap_invocation_t*
invoke_mathmap (mathmap_t *mathmap, mathmap_invocation_t *template, int img_width, int img_height)
{
    mathmap_invocation_t *invocation = (mathmap_invocation_t*)malloc(sizeof(mathmap_invocation_t));

    assert(invocation != 0);
    memset(invocation, 0, sizeof(mathmap_invocation_t));

    invocation->mathmap = mathmap;

    //invocation->variables = instantiate_variables(mathmap->variables);

    invocation_set_antialiasing(invocation, FALSE);

    invocation->supersampling = 0;

    invocation->output_bpp = 4;

    invocation->edge_behaviour_x = invocation->edge_behaviour_y = EDGE_BEHAVIOUR_COLOR;

    invocation->img_width = invocation->render_width = img_width;
    invocation->img_height = invocation->render_height = img_height;

    invocation->image_R = sqrt(2.0);

    invocation->row_stride = img_width * 4;

    invocation->rows_finished = (unsigned char*)malloc(img_height);
    memset(invocation->rows_finished, 0, img_height);

    invocation->do_debug = 0;

    invocation->uservals = instantiate_uservals(mathmap->main_filter->userval_infos, invocation);

    if (template != 0)
	carry_over_uservals_from_template(invocation, template);

    init_invocation(invocation);

    return invocation;
}

mathmap_frame_t*
invocation_new_frame (mathmap_invocation_t *invocation, image_t *closure,
		      int current_frame, float current_t)
{
    mathmap_frame_t *frame = g_new0(mathmap_frame_t, 1);

    frame->invocation = invocation;

    frame->frame_render_width = invocation->render_width;
    frame->frame_render_height = invocation->render_height;

    frame->current_frame = current_frame;
    frame->current_t = current_t;

    init_pools(&frame->pools);

    closure->v.closure.funcs->init_frame(frame, closure);

    return frame;
}

void
invocation_free_frame (mathmap_frame_t *frame)
{
    free_pools(&frame->pools);
    g_free(frame);
}

void
enable_debugging (mathmap_invocation_t *invocation)
{
    invocation->do_debug = 1;
}

void
disable_debugging (mathmap_invocation_t *invocation)
{
    invocation->do_debug = 0;
}

static void
calc_lines (mathmap_slice_t *slice, image_t *closure, int first_row, int last_row, unsigned char *q)
{
    mathmap_frame_t *frame = slice->frame;
    mathmap_invocation_t *invocation = frame->invocation;

    assert(first_row >= 0 && last_row <= invocation->img_height + 1 && first_row <= last_row);

    closure->v.closure.funcs->calc_lines(slice, closure, first_row, last_row, q, 0);
}

void
invocation_init_slice (mathmap_slice_t *slice, image_t *closure, mathmap_frame_t *frame, int region_x, int region_y,
		       int region_width, int region_height, float sampling_offset_x, float sampling_offset_y)
{
    memset(slice, 0, sizeof(mathmap_slice_t));

    slice->frame = frame;
    slice->region_x = region_x;
    slice->region_y = region_y;
    slice->region_width = region_width;
    slice->region_height = region_height;
    slice->sampling_offset_x = sampling_offset_x;
    slice->sampling_offset_y = sampling_offset_y;

    init_pools(&slice->pools);

    closure->v.closure.funcs->init_slice(slice, closure);
}

void
invocation_deinit_slice (mathmap_slice_t *slice)
{
    free_pools(&slice->pools);
}

static void
call_invocation (mathmap_frame_t *frame, image_t *closure,
		 int region_x, int region_y, int region_width, int region_height,
		 unsigned char *q)
{
    mathmap_invocation_t *invocation = frame->invocation;

    if (invocation->supersampling)
    {
	guchar *line1, *line2, *line3;
	int row, col;
	mathmap_slice_t short_slice, long_slice;

	line1 = (guchar*)malloc((region_width + 1) * invocation->output_bpp);
	line2 = (guchar*)malloc(region_width * invocation->output_bpp);
	line3 = (guchar*)malloc((region_width + 1) * invocation->output_bpp);

	invocation_init_slice(&short_slice, closure, frame, region_x, region_y, region_width, region_height, 0.0, 0.0);
	invocation_init_slice(&long_slice, closure, frame, region_x, region_y, region_width + 1, region_height, -0.5, -0.5);

	calc_lines(&long_slice, closure, region_y, region_y + 1, line1);

	for (row = region_y; row < region_y + region_height; ++row)
	{
	    unsigned char *p = q;

	    calc_lines(&short_slice, closure, row, row + 1, line2);
	    calc_lines(&long_slice, closure, row + 1, row + 2, line3);

	    for (col = 0; col < region_width; ++col)
	    {
		int i;

		for (i = 0; i < invocation->output_bpp; ++i)
		    p[i] = (line1[col*invocation->output_bpp+i]
			    + line1[(col+1)*invocation->output_bpp+i]
			    + 2*line2[col*invocation->output_bpp+i]
			    + line3[col*invocation->output_bpp+i]
			    + line3[(col+1)*invocation->output_bpp+i]) / 6;
		p += invocation->output_bpp;
	    }
	    memcpy(line1, line3, (region_width + 1) * invocation->output_bpp);

	    q += invocation->row_stride;

	    invocation->rows_finished[row] = 1;
	}

	free(line1);
	free(line2);
	free(line3);

	invocation_deinit_slice(&short_slice);
	invocation_deinit_slice(&long_slice);
    }
    else
    {
	mathmap_slice_t slice;

	invocation_init_slice(&slice, closure, frame, region_x, region_y, region_width, region_height, 0.0, 0.0);
	calc_lines(&slice, closure, region_y, region_y + region_height, q);
	invocation_deinit_slice(&slice);
    }
}

#if defined(USE_PTHREADS) || defined(USE_GTHREADS)
typedef struct
{
    thread_handle_t thread_handle;
    mathmap_frame_t *frame;
    image_t *closure;
    int region_x, region_y;
    int region_height, region_width;
    unsigned char *q;
    gboolean is_done;
} thread_data_t;

typedef struct
{
    int num_threads;
    thread_data_t datas[];
} invocation_call_t;

static void
call_invocation_thread_func (gpointer _data)
{
    thread_data_t *data = (thread_data_t*)_data;

#ifdef USE_PTHREADS
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
#endif

    call_invocation(data->frame, data->closure, data->region_x, data->region_y,
		    data->region_width, data->region_height, data->q);

    data->is_done = TRUE;
}

gpointer
call_invocation_parallel (mathmap_frame_t *frame, image_t *closure,
			  int region_x, int region_y, int region_width, int region_height,
			  unsigned char *q, int num_threads)
{
    mathmap_invocation_t *invocation = frame->invocation;
    invocation_call_t *call;
    int i;
    int first_row = region_y;
    int last_row = region_y + region_height;

    g_assert(first_row >= 0 && last_row <= invocation->img_height && first_row <= last_row);

    memset(invocation->rows_finished + first_row, 0, last_row - first_row);

    call = g_malloc(sizeof(invocation_call_t) + sizeof(thread_data_t) * num_threads);

    call->num_threads = num_threads;

    for (i = 0; i < num_threads; ++i)
    {
	call->datas[i].frame = frame;
	call->datas[i].closure = closure;
	call->datas[i].region_x = region_x;
	call->datas[i].region_width = region_width;
	call->datas[i].region_y = first_row + (last_row - first_row) * i / num_threads;
	call->datas[i].region_height = first_row + (last_row - first_row) * (i + 1) / num_threads - call->datas[i].region_y;
	call->datas[i].q = q + (call->datas[i].region_y - region_y) * invocation->row_stride;
	call->datas[i].is_done = FALSE;

	call->datas[i].thread_handle = mathmap_thread_start(call_invocation_thread_func, &call->datas[i]);
    }

    return call;
}

void
join_invocation_call (gpointer *_call)
{
    invocation_call_t *call = (invocation_call_t*)_call;
    int i;

    for (i = 0; i < call->num_threads; ++i)
	mathmap_thread_join(call->datas[i].thread_handle);

    g_free(call);
}

#ifdef USE_PTHREADS
void
kill_invocation_call (gpointer *_call)
{
    invocation_call_t *call = (invocation_call_t*)_call;
    int i;

    for (i = 0; i < call->num_threads; ++i)
	mathmap_thread_kill(call->datas[i].thread_handle);

    g_free(call);
}
#endif

gboolean
invocation_call_is_done (gpointer *_call)
{
    invocation_call_t *call = (invocation_call_t*)_call;
    int i;

    for (i = 0; i < call->num_threads; ++i)
	if (!call->datas[i].is_done)
	    return FALSE;
    return TRUE;
}

void
call_invocation_parallel_and_join (mathmap_frame_t *frame, image_t *closure,
				   int region_x, int region_y, int region_width, int region_height,
				   unsigned char *q, int num_threads)
{
    gpointer call = call_invocation_parallel(frame, closure, region_x, region_y,
					     region_width, region_height, q, num_threads);

    join_invocation_call(call);
}

#ifdef USE_PTHREAD
static void
sigusr2_handler (int signum)
{
    pthread_testcancel();
}
#endif

thread_handle_t
mathmap_thread_start (void (*func) (gpointer), gpointer data)
{
#ifdef USE_PTHREAD
    static gboolean signal_handler_set = FALSE;

    pthread_t thread;
    int result;

    if (!signal_handler_set)
    {
	signal(SIGUSR2, sigusr2_handler);
	signal_handler_set = TRUE;
    }

    result = pthread_create(&thread, NULL, (gpointer (*) (gpointer))func, data);
    g_assert(result == 0);
#else
    GThread *thread;

    if (!g_thread_supported())
	g_thread_init (NULL);

    thread = g_thread_create((gpointer (*) (gpointer))func, data, TRUE, NULL);
    g_assert(thread != NULL);
#endif

    return thread;
}

void
mathmap_thread_join (thread_handle_t thread)
{
#ifdef USE_PTHREAD
    pthread_join(thread, NULL);
#else
    g_thread_join(thread);
#endif
}

#ifdef USE_PTHREAD
void
mathmap_thread_kill (thread_handle_t thread)
{
    pthread_cancel(thread);
    pthread_kill(thread, SIGUSR2);
    pthread_join(thread, NULL);
}
#endif
#else
void
call_invocation_parallel_and_join (mathmap_frame_t *frame, image_t *closure,
				   int region_x, int region_y, int region_width, int region_height,
				   unsigned char *q, int num_threads)
{
    call_invocation(frame, closure, region_x, region_y, region_width, region_height, q);
}
#endif

void
carry_over_uservals_from_template (mathmap_invocation_t *invocation, mathmap_invocation_t *template)
{
    userval_info_t *info;

    for (info = invocation->mathmap->main_filter->userval_infos; info != 0; info = info->next)
    {
	userval_info_t *template_info = lookup_matching_userval(template->mathmap->main_filter->userval_infos, info);

	if (template_info != 0)
	    copy_userval(&invocation->uservals[info->index], &template->uservals[template_info->index], info->type);
    }
}

#define MAX_TEMPLATE_VAR_LENGTH       64
#define is_word_character(c)          (isalnum((c)) || (c) == '_')

static int
next_directive (const char *template)
{
    char *p = strchr(template, '$');

    if (p == NULL)
	return -1;

    if (p[1] == '\0')
	return -1;

    return p - template;
}

static int
find_directive (const char *template, char *name)
{
    int i = 0;
    int j;

    while ((j = next_directive(template + i)) >= 0)
    {
	if (template[i + j + 1] == '\0')
	    return -1;

	if (strncmp(template + i + j + 1, name, strlen(name)) == 0)
	    return i + j;

	i += j + 2;
    }

    return -1;
}

void
process_template (mathmap_t *mathmap, const char *template, FILE *out,
		  template_processor_func_t template_processor, void *user_data)
{
    int i = 0;
    int j;

    while ((j = next_directive(template + i)) >= 0)
    {
	if (j > 0)
	    fwrite(template + i, 1, j, out);

	i += j;

	if (!is_word_character(template[i + 1]))
	{
	    putc(template[i + 1], out);
	    i += 2;
	}
	else
	{
	    char name[MAX_TEMPLATE_VAR_LENGTH + 1];
	    int length = 1;
	    char c;
	    char *arg = 0;

	    name[0] = template[i + 1];
	    i += 2;

	    do
	    {
		c = template[i++];

		if (is_word_character(c))
		{
		    assert(length < MAX_TEMPLATE_VAR_LENGTH);
		    name[length++] = c;
		}
		else
		    if (c != '\0' && c != '$')
			--i;
	    } while (is_word_character(c));

	    assert(length > 0 && length <= MAX_TEMPLATE_VAR_LENGTH);

	    name[length] = '\0';

	    if (g_str_has_suffix(name, "_begin"))
	    {
		char *end_name = g_strdup(name);

		strcpy(end_name + length - strlen("begin"), "end");

		j = find_directive(template + i, end_name);

		g_assert(j >= 0);

		arg = g_strndup(template + i, j);

		i += j + 1 + strlen(end_name);

		g_free(end_name);
	    }

	    if (!template_processor(mathmap, name, arg, out, user_data)) {
		g_warning("Unknown template directive $%s.\n", name);
		fprintf(out, "$%s", name);
	    }
	}
    }

    fputs(template + i, out);
}

gboolean
process_template_file (mathmap_t *mathmap, char *template_filename, FILE *out,
		       template_processor_func_t template_processor, void *user_data)
{
    char *template;

    if (!g_file_get_contents(template_filename, &template, NULL, NULL))
	return FALSE;

    process_template(mathmap, template, out, template_processor, user_data);

    g_free(template);

    return TRUE;
}

int
get_num_cpus (void)
{
    static int num_cpus = 0;

    FILE *info;

    if (num_cpus != 0)
	return num_cpus;

    info = fopen("/proc/cpuinfo", "r");

    if (info != 0)
    {
	char buf[512];

	while (fgets(buf, 512, info) != 0)
	    if (strncmp(buf, "processor", strlen("processor")) == 0)
		++num_cpus;

	fclose(info);
    }

    if (num_cpus == 0)
	num_cpus = 4;

#ifdef DEBUG_OUTPUT
    g_print("have %d cpus\n", num_cpus);
#endif

    return num_cpus;
}

static designer_design_type_t*
make_mathmap_design_type (void)
{
    designer_design_type_t *type = designer_make_design_type(FALSE);

    designer_add_type(type, userval_type_name(USERVAL_INT_CONST));
    designer_add_type(type, userval_type_name(USERVAL_FLOAT_CONST));
    designer_add_type(type, userval_type_name(USERVAL_BOOL_CONST));
    designer_add_type(type, userval_type_name(USERVAL_COLOR));
    designer_add_type(type, userval_type_name(USERVAL_CURVE));
    designer_add_type(type, userval_type_name(USERVAL_GRADIENT));
    designer_add_type(type, userval_type_name(USERVAL_IMAGE));

    return type;
}

static void
remove_edb (expression_db_t **edb)
{
    expression_db_t *e = *edb;

    g_assert(e != NULL);

    *edb = e->next;

    e->next = NULL;
    free_expression_db(e);
}

static void
add_filter_node_type (designer_design_type_t *design_type, expression_db_t **edb)
{
    char *name = get_expression_name(*edb, design_type);
    userval_info_t *args = get_expression_args(*edb, design_type);
    designer_node_type_t *type = designer_add_node_type(design_type, name, *edb);

    if (type == NULL)
    {
	char *message;

	type = designer_get_node_type_by_name(design_type, name);
	g_assert(type != NULL);

	message = g_strdup_printf(_("The filters in the files\n"
				    "`%s'\n"
				    "and\n"
				    "`%s'\n"
				    "have the same name `%s'.\n"
				    "Only the former can be used from the composer."),
				  get_expression_path(type->data),
				  get_expression_path(*edb),
				  name);
	mathmap_message_dialog(message);
	g_free(message);

	remove_edb(edb);

	return;
    }

    while (args != NULL)
    {
	if (args->type == USERVAL_IMAGE)
	    designer_add_input_slot_spec(type, args->name, userval_type_name(args->type), args);
	args = args->next;
    }

    designer_add_output_slot_spec(type, "out", userval_type_name(USERVAL_IMAGE), NULL);
}

static void
add_node_types (designer_design_type_t *design_type, expression_db_t **edb, gboolean compositions, gboolean *did_add)
{
    while (*edb != NULL)
    {
	expression_db_t *current = *edb;

	switch ((*edb)->kind)
	{
	    case EXPRESSION_DB_EXPRESSION :
		if (!compositions)
		{
		    char *name = get_expression_name(*edb, design_type);

		    if (name != NULL)
			add_filter_node_type(design_type, edb);
		    else
		    {
			char *message;

			message = g_strdup_printf(_("The filter in the file\n"
						    "`%s'\n"
						    "cannot be parsed."),
						  get_expression_path(*edb));
			mathmap_message_dialog(message);
			g_free(message);

			remove_edb(edb);
		    }
		}
		break;

	    case EXPRESSION_DB_GROUP :
		add_node_types(design_type, &(*edb)->v.group.subs, compositions, did_add);
		break;

	    case EXPRESSION_DB_DESIGN :
		if (compositions)
		{
		    designer_design_t *design = designer_load_design(design_type, (*edb)->v.design.path,
								     NULL, NULL, NULL, NULL);
		    char *name = get_expression_name ((*edb), design_type);

		    if (design == NULL)
		    {
			char *message;

			message = g_strdup_printf(_("Could not load composition from file\n"
						    "`%s'.\n"),
						  get_expression_path(*edb));
			mathmap_message_dialog(message);
			g_free(message);

			remove_edb(edb);
			break;
		    }

		    if (designer_get_node_type_by_name(design_type, design->name))
		    {
			designer_free_design(design);
			break;
		    }

		    designer_free_design(design);

		    if (name == NULL)
		    {
			char *message;

			message = g_strdup_printf(_("The composition in the file\n"
						    "`%s'\n"
						    "does not produce a valid MathMap\n"
						    "filter.  Please check that all filters\n"
						    "used by that composition work properly."),
						  get_expression_path(*edb));
			mathmap_message_dialog(message);
			g_free(message);

			remove_edb(edb);
			break;
		    }

		    add_filter_node_type(design_type, edb);

		    *did_add = TRUE;
		}
		break;

	    default :
		g_assert_not_reached();
	}

	if (*edb == current)
	    edb = &(*edb)->next;
    }
}

designer_design_type_t*
design_type_from_expression_db (expression_db_t **edb)
{
    designer_design_type_t *type = make_mathmap_design_type();
    gboolean did_add;

    add_node_types(type, edb, FALSE, NULL);
    do
    {
	did_add = FALSE;
	add_node_types(type, edb, TRUE, &did_add);
    } while (did_add);

    return type;
}

void
init_gettext (void)
{
    setlocale(LC_ALL, "");
    textdomain("mathmap");
    bindtextdomain("mathmap", LOCALEDIR);
}
