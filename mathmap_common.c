/* -*- c -*- */

/*
 * mathmap_common.c
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

#include <math.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include "internals.h"
#include "tags.h"
#include "jump.h"
#include "scanner.h"
#include "compiler.h"
#include "mathmap.h"

int cmd_line_mode = 0; 

mathmap_t *the_mathmap = 0;
int scanner_line_num;

/* from parser.y */
int yyparse (void);

static unsigned int
image_flags_from_options (option_t *options)
{
    unsigned int flags = 0;
    option_t *unit_option = find_option_with_name(options, "unit");

    if (unit_option != 0)
    {
	flags |= IMAGE_FLAG_UNIT;
	if (find_option_with_name(unit_option->suboptions, "stretched") == 0)
	    flags |= IMAGE_FLAG_SQUARE;
    }

    return flags;
}

void
register_args_as_uservals (filter_t *filter, arg_decl_t *arg_decls)
{
    while (arg_decls != 0)
    {
	userval_info_t *result = 0;

	//printf("registering %s  type %d\n", arg_decls->name, arg_decls->type);

	switch (arg_decls->type)
	{
	    case ARG_TYPE_INT :
		if (arg_decls->v.integer.have_limits)
		    result = register_int_const(&filter->userval_infos, arg_decls->name,
						arg_decls->v.integer.min, arg_decls->v.integer.max,
						arg_decls->v.integer.default_value);
		else
		    result = register_int_const(&filter->userval_infos, arg_decls->name, -100000, 100000, 0);
		break;

	    case ARG_TYPE_FLOAT :
		if (arg_decls->v.floating.have_limits)
		    result = register_float_const(&filter->userval_infos, arg_decls->name,
						  arg_decls->v.floating.min, arg_decls->v.floating.max,
						  arg_decls->v.floating.default_value);
		else
		    result = register_float_const(&filter->userval_infos, arg_decls->name, -1.0, 1.0, 0.0);
		break;

	    case ARG_TYPE_BOOL :
		result = register_bool(&filter->userval_infos, arg_decls->name, arg_decls->v.boolean.default_value);
		break;

	    case ARG_TYPE_COLOR :
		result = register_color(&filter->userval_infos, arg_decls->name);
		break;

	    case ARG_TYPE_GRADIENT :
		result = register_gradient(&filter->userval_infos, arg_decls->name);
		break;

	    case ARG_TYPE_CURVE :
		result = register_curve(&filter->userval_infos, arg_decls->name);
		break;

	    case ARG_TYPE_FILTER :
		assert(0);

	    case ARG_TYPE_IMAGE :
		result = register_image(&filter->userval_infos, arg_decls->name,
					image_flags_from_options(arg_decls->options));
		break;

	    default :
		assert(0);
	}

	if (result == 0)
	{
	    sprintf(error_string, "Conflict for argument %s.", arg_decls->name);
	    JUMP(1);
	}

	arg_decls = arg_decls->next;
    }
}

static void
init_internals (filter_t *filter)
{
    register_internal(&filter->internals, "x", CONST_Y | CONST_T);
    register_internal(&filter->internals, "y", CONST_X | CONST_T);
    register_internal(&filter->internals, "r", CONST_T);
    register_internal(&filter->internals, "a", CONST_T);
    register_internal(&filter->internals, "t", CONST_X | CONST_Y);
    register_internal(&filter->internals, "X", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->internals, "Y", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->internals, "W", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->internals, "H", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->internals, "R", CONST_X | CONST_Y | CONST_T);
    register_internal(&filter->internals, "frame", CONST_X | CONST_Y);
}

void
start_parsing_filter (mathmap_t *mathmap)
{
    g_assert(mathmap->current_filter == 0);

    mathmap->current_filter = g_new0(filter_t, 1);

    init_internals(mathmap->current_filter);
}

void
finish_parsing_filter (mathmap_t *mathmap)
{
    filter_t *filter = mathmap->current_filter;
    userval_info_t *info;

    g_assert(filter != 0);

    filter->num_uservals = 0;
    for (info = filter->userval_infos; info != 0; info = info->next)
	++filter->num_uservals;

    filter->next = mathmap->filters;
    mathmap->filters = filter;

    mathmap->current_filter = 0;
}

static void
free_filters (filter_t *filter)
{
    while (filter != 0)
    {
	filter_t *next = filter->next;

	if (filter->variables != 0)
	    free_variables(filter->variables);
	if (filter->userval_infos != 0)
	    free_userval_infos(filter->userval_infos);

	g_free(filter);

	filter = next;
    }
}

void
unload_mathmap (mathmap_t *mathmap)
{
    if ((mathmap->flags & MATHMAP_FLAG_NATIVE) && mathmap->module_info != 0)
    {
	unload_c_code(mathmap->module_info);
	mathmap->module_info = 0;
    }
}

void
free_mathmap (mathmap_t *mathmap)
{
    if (mathmap->filters != 0)
	free_filters(mathmap->filters);
    if (mathmap->interpreter_insns != 0)
	free(mathmap->interpreter_insns);
    if (mathmap->interpreter_values != 0)
	g_array_free(mathmap->interpreter_values, 1);
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

#define X_INTERNAL_INDEX         0
#define Y_INTERNAL_INDEX         1
#define R_INTERNAL_INDEX         2
#define A_INTERNAL_INDEX         3

int
does_filter_use_ra (filter_t *filter)
{
    internal_t *r_internal = lookup_internal(filter->internals, "r", 1);
    internal_t *a_internal = lookup_internal(filter->internals, "a", 1);

    assert(r_internal != 0 && a_internal != 0);

    return r_internal->is_used || a_internal->is_used;
}

int
does_filter_use_t (filter_t *filter)
{
    internal_t *t_internal = lookup_internal(filter->internals, "t", 1);

    assert(t_internal != 0);

    return t_internal->is_used;
}

mathmap_t*
parse_mathmap (char *expression)
{
    static mathmap_t *mathmap;	/* this is static to avoid problems with longjmp.  */
    exprtree *expr;

    mathmap = g_new0(mathmap_t, 1);

    the_mathmap = mathmap;

    DO_JUMP_CODE {
	scanner_line_num = 0;
	scanFromString(expression);
	yyparse();
	endScanningFromString();

	if (mathmap->filters == 0
	    || mathmap->filters->next != 0
	    || mathmap->filters->decl->type != TOP_LEVEL_FILTER)
	{
	    free_filters(mathmap->filters);
	    mathmap->filters = 0;

	    sprintf(error_string, "Exactly one filter must be defined.");
	    JUMP(1);
	}

	mathmap->main_filter = mathmap->filters;

	expr = mathmap->main_filter->decl->v.filter.body;

	if (expr->result.number != rgba_tag_number
	    || expr->result.length != 4)
	{
	    free_filters(mathmap->filters);
	    mathmap->filters = 0;

	    sprintf(error_string, "The expression must have the result type rgba:4.");
	    JUMP(1);
	}

	mathmap->flags = image_flags_from_options(mathmap->main_filter->decl->v.filter.options);
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
    mathmap_t *mathmap = parse_mathmap(expression);

    if (mathmap != 0)
    {
	free_mathmap(mathmap);
	return 1;
    }
    else
	return 0;
}

mathmap_t*
compile_mathmap (char *expression, char *template_filename, char *opmacros_filename)
{
    static mathmap_t *mathmap;	/* this is static to avoid problems with longjmp.  */
    static int try_compiler = 1;

    DO_JUMP_CODE {
	if (try_compiler)
	{
	    mathmap = parse_mathmap(expression);

	    if (mathmap == 0)
	    {
		JUMP(1);
	    }

	    mathmap->initfunc = gen_and_load_c_code(mathmap, &mathmap->module_info, template_filename, opmacros_filename);
	    if (mathmap->initfunc == 0 && !cmd_line_mode)
	    {
		char *message = g_strdup_printf("The MathMap compiler failed.  This is not a fatal error,\n"
						"because MathMap has a fallback interpreter which can do\n"
						"everything the compiler does, but it is much slower, so it\n"
						"is recommended that you use the compiler.\n"
						"This is the reason why the compiler failed:\n%s", error_string);

		gimp_message(message);

		g_free(message);
	    }
	}

	if (!try_compiler || mathmap->initfunc == 0)
	{
	    mathmap = parse_mathmap(expression);

	    if (mathmap == 0)
	    {
		JUMP(1);
	    }

	    generate_interpreter_code(mathmap);

	    mathmap->flags &= ~MATHMAP_FLAG_NATIVE;

	    try_compiler = 0;
	}
	else
	    mathmap->flags |= MATHMAP_FLAG_NATIVE;

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

static void
init_invocation (mathmap_invocation_t *invocation)
{
    if (invocation->mathmap->flags & MATHMAP_FLAG_NATIVE)
	invocation->mathfuncs = invocation->mathmap->initfunc(invocation);
    else
    {
	/*
	int i;

	invocation->interpreter_values = g_array_new(FALSE, TRUE, sizeof(runtime_value_t));
	g_array_set_size(invocation->interpreter_values, invocation->mathmap->interpreter_values->len);
	for (i = 0; i < invocation->mathmap->interpreter_values->len; ++i)
	    g_array_index(invocation->interpreter_values, runtime_value_t, i)
		= g_array_index(invocation->mathmap->interpreter_values, runtime_value_t, i);
	*/
    }
}

/* The scale factors computed by this function are to get from virtual
   coordinates to pixel coordinates. */
void
calc_scale_factors (unsigned int flags, int pixel_width, int pixel_height, float *scale_x, float *scale_y)
{
    float virt_width, virt_height;

    switch (flags & (IMAGE_FLAG_UNIT | IMAGE_FLAG_SQUARE))
    {
	case 0 :
	    virt_width = pixel_width;
	    virt_height = pixel_height;
	    break;

	case IMAGE_FLAG_UNIT :
	    virt_width = virt_height = 2.0;
	    break;

	case IMAGE_FLAG_UNIT | IMAGE_FLAG_SQUARE :
	    if (pixel_width > pixel_height)
	    {
		virt_width = 2.0;
		virt_height = 2.0 * pixel_height / pixel_width;
	    }
	    else
	    {
		virt_height = 2.0;
		virt_width = 2.0 * pixel_width / pixel_height;
	    }
	    break;

	default :
	    assert(0);
    }

    *scale_x = pixel_width / virt_width;
    *scale_y = pixel_height / virt_height;
}

void
calc_middle_values (int img_width, int img_height, float scale_x, float scale_y, float *middle_x, float *middle_y)
{
    *middle_x = (img_width - 1) / 2.0 * scale_x;
    *middle_y = (img_height - 1) / 2.0 * scale_y;
}

mathmap_invocation_t*
invoke_mathmap (mathmap_t *mathmap, mathmap_invocation_t *template, int img_width, int img_height)
{
    mathmap_invocation_t *invocation = (mathmap_invocation_t*)malloc(sizeof(mathmap_invocation_t));

    assert(invocation != 0);
    memset(invocation, 0, sizeof(mathmap_invocation_t));

    invocation->mathmap = mathmap;

    //invocation->variables = instantiate_variables(mathmap->variables);

    invocation->antialiasing = 0;
    invocation->supersampling = 0;

    invocation->output_bpp = 4;

    invocation->edge_behaviour_x = invocation->edge_behaviour_y = EDGE_BEHAVIOUR_COLOR;

    invocation->img_width = img_width;
    invocation->img_height = img_height;

    calc_scale_factors(mathmap->flags, img_width, img_height, &invocation->scale_x, &invocation->scale_y);
    invocation->scale_x = 1.0 / invocation->scale_x;
    invocation->scale_y = 1.0 / invocation->scale_y;

    invocation->image_W = img_width * invocation->scale_x;
    invocation->image_H = img_height * invocation->scale_y;

    calc_middle_values(img_width, img_height,
		       invocation->scale_x, invocation->scale_y,
		       &invocation->middle_x, &invocation->middle_y);

    invocation->image_X = invocation->middle_x;
    invocation->image_Y = invocation->middle_y;
    
    invocation->image_R = hypot(invocation->image_X, invocation->image_Y);

    invocation->current_r = invocation->current_a = 0.0;

    invocation->row_stride = img_width * 4;

    invocation->rows_finished = (unsigned char*)malloc(img_height);
    memset(invocation->rows_finished, 0, img_height);

    invocation->do_debug = 0;

    invocation->uservals = instantiate_uservals(mathmap->main_filter->userval_infos, invocation);

#ifndef OPENSTEP
    if (!cmd_line_mode)
    {
	userval_info_t *info;

	// we give the original image as the first input image
	for (info = mathmap->main_filter->userval_infos; info != 0; info = info->next)
	    if (info->type == USERVAL_IMAGE)
	    {
		assign_image_userval_drawable(info, &invocation->uservals[info->index],
					      copy_input_drawable(get_default_input_drawable()));
		break;
	    }
    }
#endif

    if (template != 0)
	carry_over_uservals_from_template(invocation, template);

    init_invocation(invocation);

    return invocation;
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
set_float_internal (mathmap_invocation_t *invocation, int index, float value)
{
    g_array_index(invocation->mathmap->interpreter_values, runtime_value_t, index).float_value = value;
}

void
update_image_internals (mathmap_invocation_t *invocation)
{
    internal_t *internal;

    if (invocation->mathmap->flags & MATHMAP_FLAG_NATIVE)
	return;

    internal = lookup_internal(invocation->mathmap->main_filter->internals, "t", 1);
    set_float_internal(invocation, internal->index, invocation->current_t);

    internal = lookup_internal(invocation->mathmap->main_filter->internals, "X", 1);
    set_float_internal(invocation, internal->index, invocation->image_X);
    internal = lookup_internal(invocation->mathmap->main_filter->internals, "Y", 1);
    set_float_internal(invocation, internal->index, invocation->image_Y);
    
    internal = lookup_internal(invocation->mathmap->main_filter->internals, "W", 1);
    set_float_internal(invocation, internal->index, invocation->image_W);
    internal = lookup_internal(invocation->mathmap->main_filter->internals, "H", 1);
    set_float_internal(invocation, internal->index, invocation->image_H);
    
    internal = lookup_internal(invocation->mathmap->main_filter->internals, "R", 1);
    set_float_internal(invocation, internal->index, invocation->image_R);

    internal = lookup_internal(invocation->mathmap->main_filter->internals, "frame", 1);
    set_float_internal(invocation, internal->index, invocation->current_frame);
}

static void
update_pixel_internals (mathmap_invocation_t *invocation, float x, float y, float r, float a)
{
    set_float_internal(invocation, X_INTERNAL_INDEX, x);
    set_float_internal(invocation, Y_INTERNAL_INDEX, y);

    set_float_internal(invocation, R_INTERNAL_INDEX, r);
    set_float_internal(invocation, A_INTERNAL_INDEX, a);
}

static void
write_color_to_pixel (color_t color, guchar *dest, int output_bpp)
{
    if (output_bpp == 1 || output_bpp == 2)
	dest[0] = (RED(color) * 299 + GREEN(color) * 587 + BLUE(color) * 114) / 1000;
    else if (output_bpp == 3 || output_bpp == 4)
    {
	dest[0] = RED(color);
	dest[1] = GREEN(color);
	dest[2] = BLUE(color);
    }
    else
	assert(0);

    if (output_bpp == 2 || output_bpp == 4)
	dest[output_bpp - 1] = ALPHA(color);
}

void
init_frame (mathmap_slice_t *slice)
{
    if (slice->invocation->mathmap->flags & MATHMAP_FLAG_NATIVE)
	slice->invocation->mathfuncs.init_frame(slice);
}

static void
run_interpreter (mathmap_invocation_t *invocation)
{
    invocation->interpreter_ip = 0;
    do
    {
	interpreter_insn_t *insn = &invocation->mathmap->interpreter_insns[invocation->interpreter_ip];

	++invocation->interpreter_ip;
	insn->func(invocation, insn->arg_indexes);
    } while (invocation->interpreter_ip >= 0);
}

static void
calc_lines (mathmap_slice_t *slice, int first_row, int last_row, unsigned char *q)
{
    mathmap_invocation_t *invocation = slice->invocation;

    assert(first_row >= 0 && last_row <= invocation->img_height + 1 && first_row <= last_row);

    if (invocation->mathmap->flags & MATHMAP_FLAG_NATIVE)
	invocation->mathfuncs.calc_lines(slice, first_row, last_row, q);
    else
    {
	int row, col;
	int output_bpp = invocation->output_bpp;
	int origin_x = slice->region_x, origin_y = slice->region_y;
	float middle_x = invocation->middle_x, middle_y = invocation->middle_y;
	float sampling_offset_x = slice->sampling_offset_x, sampling_offset_y = slice->sampling_offset_y;
	float scale_x = invocation->scale_x, scale_y = invocation->scale_y;
	int uses_ra = does_filter_use_ra(invocation->mathmap->main_filter);

	for (row = first_row; row < last_row; ++row)
	{
	    float y = CALC_VIRTUAL_Y(row, origin_y, scale_y, middle_y, sampling_offset_y);
	    unsigned char *p = q;

	    for (col = 0; col < slice->region_width; ++col)
	    {
		float x = CALC_VIRTUAL_X(col, origin_x, scale_x, middle_x, sampling_offset_x);
		float r, a;

		if (uses_ra)
		{
		    r = hypot(x, y);
		    if (r == 0.0)
			a = 0.0;
		    else
			a = acos(x / r);

		    if (y < 0)
			a = 2 * M_PI - a;
		}
		else
		    r = a = 0.0; /* to make the compiler happy */

		update_pixel_internals(invocation, x, y, r, a);

		run_interpreter(invocation);

		write_color_to_pixel(invocation->interpreter_output_color, p, output_bpp);

		p += output_bpp;
	    }

	    q += invocation->row_stride;

	    if (!invocation->supersampling)
		invocation->rows_finished[row] = 1;
	}
    }
}

static void
init_slice (mathmap_slice_t *slice, mathmap_invocation_t *invocation, int region_x, int region_y,
	    int region_width, int region_height, float sampling_offset_x, float sampling_offset_y)
{
    memset(slice, 0, sizeof(mathmap_slice_t));

    slice->invocation = invocation;
    slice->region_x = region_x;
    slice->region_y = region_y;
    slice->region_width = region_width;
    slice->region_height = region_height;
    slice->sampling_offset_x = sampling_offset_x;
    slice->sampling_offset_y = sampling_offset_y;

    init_frame(slice);
}

static void
deinit_slice (mathmap_slice_t *slice)
{
    if (slice->xy_vars)
	free(slice->xy_vars);
    if (slice->y_vars)
	free(slice->y_vars);
}

static void
call_invocation (mathmap_invocation_t *invocation, int region_x, int region_y, int region_width, int region_height,
		 unsigned char *q)
{
    if (invocation->supersampling)
    {
	guchar *line1, *line2, *line3;
	int row, col;
	mathmap_slice_t short_slice, long_slice;

	line1 = (guchar*)malloc((region_width + 1) * invocation->output_bpp);
	line2 = (guchar*)malloc(region_width * invocation->output_bpp);
	line3 = (guchar*)malloc((region_width + 1) * invocation->output_bpp);

	init_slice(&short_slice, invocation, region_x, region_y, region_width, region_height, 0.0, 0.0);
	init_slice(&long_slice, invocation, region_x, region_y, region_width + 1, region_height, -0.5, -0.5);

	calc_lines(&long_slice, region_y, region_y + 1, line1);

	for (row = region_y; row < region_y + region_height; ++row)
	{
	    unsigned char *p = q;

	    calc_lines(&short_slice, row, row + 1, line2);
	    calc_lines(&long_slice, row + 1, row + 2, line3);

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

	deinit_slice(&short_slice);
	deinit_slice(&long_slice);
    }
    else
    {
	mathmap_slice_t slice;

	init_slice(&slice, invocation, region_x, region_y, region_width, region_height, 0.0, 0.0);
	calc_lines(&slice, region_y, region_y + region_height, q);
	deinit_slice(&slice);
    }
}

typedef struct
{
    mathmap_invocation_t *invocation;
    int region_x, region_y;
    int region_height, region_width;
    unsigned char *q;
} thread_data_t;

static gpointer
call_invocation_thread_func (gpointer _data)
{
    thread_data_t *data = (thread_data_t*)_data;

    call_invocation (data->invocation, data->region_x, data->region_y, data->region_width, data->region_height, data->q);

    return NULL;
}

void
call_invocation_parallel (mathmap_invocation_t *invocation,
			  int region_x, int region_y, int region_width, int region_height,
			  unsigned char *q, int num_threads)
{
    GThread *threads[num_threads];
    thread_data_t datas[num_threads];
    int i;
    int first_row = region_y;
    int last_row = region_y + region_height;

    assert(first_row >= 0 && last_row <= invocation->img_height && first_row <= last_row);

    memset(invocation->rows_finished + first_row, 0, last_row - first_row);

    if (num_threads < 2 || !(invocation->mathmap->flags & MATHMAP_FLAG_NATIVE))
    {
	call_invocation (invocation, region_x, region_y, region_width, region_height, q);
	return;
    }

    if (!g_thread_supported())
	g_thread_init(NULL);

    for (i = 0; i < num_threads; ++i)
    {
	datas[i].invocation = invocation;
	datas[i].region_x = region_x;
	datas[i].region_width = region_width;
	datas[i].region_y = first_row + (last_row - first_row) * i / num_threads;
	datas[i].region_height = first_row + (last_row - first_row) * (i + 1) / num_threads - datas[i].region_y;
	datas[i].q = q + (datas[i].region_y - region_y) * invocation->row_stride;

	//call_invocation_thread_func (&datas[i]);

	if (i > 0) {
	    threads[i] = g_thread_create(call_invocation_thread_func, &datas[i], TRUE, NULL);

	    g_assert (threads[i] != NULL);
	}
    }

    call_invocation_thread_func (&datas[0]);

    for (i = 1; i < num_threads; ++i)
	g_thread_join(threads[i]);
}

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

		g_print("looking for %s\n", end_name);

		j = find_directive(template + i, end_name);

		g_assert(j >= 0);

		arg = strndup(template + i, j);

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
