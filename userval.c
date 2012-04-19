/*
 * userval.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2010 Mark Probst
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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "mathmap.h"
#include "userval.h"
#include "tags.h"
#include "drawable.h"

static userval_info_t*
alloc_and_register_userval (userval_info_t **p, const char *name, int type)
{
    userval_info_t *info = (userval_info_t*)malloc(sizeof(userval_info_t));

    memset(info, 0, sizeof(userval_info_t));

    info->name = strdup(name);
    assert(name != 0);

    info->type = type;
    info->next = 0;

    info->index = 0;
    while (*p != 0)
    {
	p = &(*p)->next;
	++info->index;
    }
    *p = info;

    return info;
}

userval_info_t*
lookup_userval (userval_info_t *infos, const char *name)
{
    userval_info_t *info;

    for (info = infos; info != 0; info = info->next)
	if (strcmp(name, info->name) == 0)
	    return info;

    return 0;
}

userval_info_t*
lookup_matching_userval (userval_info_t *infos, userval_info_t *test_info)
{
    userval_info_t *info = lookup_userval(infos, test_info->name);

    if (info == 0 || info->type != test_info->type)
	return 0;

    switch (info->type)
    {
	case USERVAL_INT_CONST :
	    if (info->v.int_const.min != test_info->v.int_const.min
		|| info->v.int_const.max != test_info->v.int_const.max)
		info = 0;
	    break;

	case USERVAL_FLOAT_CONST :
	    if (info->v.float_const.min != test_info->v.float_const.min
		|| info->v.float_const.max != test_info->v.float_const.max)
		info = 0;
	    break;

	case USERVAL_IMAGE :
	    if (info->v.image.flags != test_info->v.image.flags)
		info = 0;
	    break;
    }

    return info;
}

userval_info_t*
register_int_const (userval_info_t **infos, const char *name, int min, int max, int default_value)
{
    userval_info_t *info;

    assert(default_value >= min && default_value <= max);

    info = lookup_userval(*infos, name);
    if (info != 0)
    {
	if (info->type != USERVAL_INT_CONST)
	    return 0;
	if (info->v.int_const.min == min && info->v.int_const.max == max)
	{
	    info->v.int_const.default_value = default_value;
	    return info;
	}
	return 0;
    }
    else
    {
	info = alloc_and_register_userval(infos, name, USERVAL_INT_CONST);
	info->v.int_const.min = min;
	info->v.int_const.max = max;
	info->v.int_const.default_value = default_value;
    }

    return info;
}

userval_info_t*
register_float_const (userval_info_t **infos, const char *name, float min, float max, float default_value)
{
    userval_info_t *info;

    assert(default_value >= min && default_value <= max);

    info = lookup_userval(*infos, name);
    if (info != 0)
    {
	if (info->type != USERVAL_FLOAT_CONST)
	    return 0;
	if (info->v.float_const.min == min && info->v.float_const.max == max)
	{
	    info->v.float_const.default_value = default_value;
	    return info;
	}
	return 0;
    }
    else
    {
	info = alloc_and_register_userval(infos, name, USERVAL_FLOAT_CONST);
	info->v.float_const.min = min;
	info->v.float_const.max = max;
	info->v.float_const.default_value = default_value;
    }

    return info;
}

userval_info_t*
register_bool (userval_info_t **infos, const char *name, int default_value)
{
    userval_info_t *info;

    info = lookup_userval(*infos, name);
    if (info != 0)
    {
	if (info->type != USERVAL_BOOL_CONST)
	    return 0;
	return info;
    }

    info = alloc_and_register_userval(infos, name, USERVAL_BOOL_CONST);
    info->v.bool_const.default_value = default_value;

    return info;
}

userval_info_t*
register_color (userval_info_t **infos, const char *name)
{
    userval_info_t *info;

    info = lookup_userval(*infos, name);
    if (info != 0)
    {
	if (info->type != USERVAL_COLOR)
	    return 0;
	return info;
    }

    info = alloc_and_register_userval(infos, name, USERVAL_COLOR);

    return info;
}

userval_info_t*
register_curve (userval_info_t **infos, const char *name)
{
    userval_info_t *info;

    info = lookup_userval(*infos, name);
    if (info != 0)
    {
	if (info->type != USERVAL_CURVE)
	    return 0;
	return info;
    }

    info = alloc_and_register_userval(infos, name, USERVAL_CURVE);

    return info;
}

userval_info_t*
register_gradient (userval_info_t **infos, const char *name)
{
    userval_info_t *info;

    info = lookup_userval(*infos, name);
    if (info != 0)
    {
	if (info->type != USERVAL_GRADIENT)
	    return 0;
	return info;
    }

    info = alloc_and_register_userval(infos, name, USERVAL_GRADIENT);

    return info;
}

userval_info_t*
register_image (userval_info_t **infos, const char *name, unsigned int flags)
{
    userval_info_t *info;

    info = lookup_userval(*infos, name);
    if (info != 0)
    {
	if (info->type != USERVAL_IMAGE)
	    return 0;
	if (info->v.image.flags != flags)
	    return 0;
	return info;
    }

    info = alloc_and_register_userval(infos, name, USERVAL_IMAGE);

    info->v.image.flags = flags;

    return info;
}

static void
calc_image_values (userval_info_t *info, userval_t *val)
{
    int width, height;

    g_assert(info->type == USERVAL_IMAGE);
    g_assert(val->v.image->type == IMAGE_DRAWABLE);
    g_assert(val->v.image->v.drawable != NULL);

    width = val->v.image->pixel_width;
    height = val->v.image->pixel_height;

    val->v.image->v.drawable->scale_x = (width - 1) / 2.0;
    val->v.image->v.drawable->scale_y = (height - 1) / 2.0;

    val->v.image->v.drawable->middle_x = 1.0;
    val->v.image->v.drawable->middle_y = 1.0;
}

static curve_t*
get_default_curve (void)
{
    static curve_t curve;
    static double xs[2], ys[2];
    static float points[USER_CURVE_POINTS];
    static gboolean inited = FALSE;

    if (!inited)
    {
	int i;

	xs[0] = ys[0] = 0.0;
	xs[1] = ys[1] = 1.0;

	for (i = 0; i < USER_CURVE_POINTS; ++i)
	    points[i] = (float)i / (float)(USER_CURVE_POINTS - 1);

	curve.num_control_points = 2;
	curve.control_xs = xs;
	curve.control_ys = ys;

	curve.values = points;

	inited = TRUE;
    }

    return &curve;
}

static curve_t*
copy_curve (curve_t *curve)
{
    curve_t *copy = g_new(curve_t, 1);

    copy->num_control_points = curve->num_control_points;

    copy->control_xs = g_new(double, copy->num_control_points);
    memcpy(copy->control_xs, curve->control_xs, sizeof(double) * copy->num_control_points);

    copy->control_ys = g_new(double, copy->num_control_points);
    memcpy(copy->control_ys, curve->control_ys, sizeof(double) * copy->num_control_points);

    copy->values = g_new(float, USER_CURVE_POINTS);
    memcpy(copy->values, curve->values, sizeof(float) * USER_CURVE_POINTS);

    return copy;
}

static gradient_t*
copy_gradient (gradient_t *gradient)
{
    gradient_t *copy = g_new(gradient_t, 1);

    copy->values = g_new(color_t, USER_GRADIENT_POINTS);
    memcpy(copy->values, gradient->values, sizeof(color_t) * USER_GRADIENT_POINTS);

    return copy;
}

static void
free_curve (curve_t *curve)
{
    g_assert(curve != get_default_curve());

    g_free(curve->control_xs);
    g_free(curve->control_ys);
    g_free(curve->values);
    g_free(curve);
}

static void
free_gradient (gradient_t *gradient)
{
    g_assert(gradient != get_default_gradient());

    g_free(gradient->values);
    g_free(gradient);
}

void
set_userval_to_default (userval_t *val, userval_info_t *info, mathmap_invocation_t *invocation)
{
    switch (info->type)
    {
	case USERVAL_INT_CONST :
	    val->v.int_const = info->v.int_const.default_value;
	    break;

	case USERVAL_FLOAT_CONST :
	    val->v.float_const = info->v.float_const.default_value;
	    break;

	case USERVAL_BOOL_CONST :
	    val->v.bool_const = info->v.bool_const.default_value;
	    break;

	case USERVAL_CURVE :
	    val->v.curve = copy_curve(get_default_curve());
	    break;

	case USERVAL_GRADIENT :
	    val->v.gradient = copy_gradient(get_default_gradient());
	    break;

	case USERVAL_COLOR :
	    val->v.color = COLOR_BLACK;
	    break;

	case USERVAL_IMAGE :
	    {
		input_drawable_t *drawable = get_default_input_drawable();

		if (drawable != NULL)
		    assign_image_userval_drawable(info, val,
						  copy_input_drawable(drawable));
		else
		    val->v.image = NULL;
	    }
	    break;
    }
}

void
instantiate_userval (userval_t *val, userval_info_t *info, mathmap_invocation_t *invocation)
{
    set_userval_to_default(val, info, invocation);
}

userval_t*
instantiate_uservals (userval_info_t *infos, mathmap_invocation_t *invocation)
{
    int n;
    userval_info_t *info;
    userval_t *uservals;

    n = 0;
    for (info = infos; info != 0; info = info->next)
	++n;

    uservals = (userval_t*)malloc(n * sizeof(userval_t));
    memset(uservals, 0, n * sizeof(userval_t));

    for (info = infos; info != 0; info = info->next)
	instantiate_userval(&uservals[info->index], info, invocation);

    return uservals;
}

void
free_uservals (userval_t *uservals, userval_info_t *infos)
{
    userval_info_t *info;

    for (info = infos; info != 0; info = info->next)
    {
	switch (info->type)
	{
	    case USERVAL_CURVE :
		free_curve(uservals[info->index].v.curve);
		break;

	    case USERVAL_GRADIENT :
		free_gradient(uservals[info->index].v.gradient);
		break;

	    case USERVAL_IMAGE :
		g_assert(uservals[info->index].v.image != NULL);
		if (uservals[info->index].v.image->type == IMAGE_DRAWABLE) {
		    g_assert(uservals[info->index].v.image->v.drawable);
		    free_input_drawable(uservals[info->index].v.image->v.drawable);
		}
		break;
	}
    }
}

void
free_userval_infos (userval_info_t *infos)
{
    while (infos != 0)
    {
	userval_info_t *next = infos->next;

	free(infos->name);
	free(infos);

	infos = next;
    }
}

void
copy_userval (userval_t *dst, userval_t *src, int type)
{
    switch (type)
    {
	case USERVAL_INT_CONST :
	case USERVAL_FLOAT_CONST :
	case USERVAL_BOOL_CONST :
	case USERVAL_COLOR :
	    dst->v = src->v;
	    break;

	case USERVAL_CURVE :
	    dst->v.curve = copy_curve(src->v.curve);
	    break;

	case USERVAL_GRADIENT :
	    dst->v.gradient = copy_gradient(src->v.gradient);
	    break;

	case USERVAL_IMAGE :
	    {
		input_drawable_t *dst_drawable;

		g_assert (src->v.image != NULL);
		g_assert (src->v.image->type == IMAGE_DRAWABLE);

		if (dst->v.image->type == IMAGE_DRAWABLE)
		    dst_drawable = dst->v.image->v.drawable;
		else
		    dst_drawable = NULL;

		if (src->v.image->v.drawable != 0)
		{
		    input_drawable_t *copy = copy_input_drawable(src->v.image->v.drawable);

		    dst->v.image = &copy->image;
		}
		else
		    dst->v.image = 0;

		if (dst_drawable != 0)
		    free_input_drawable(dst_drawable);
	    }
	    break;

	default :
	    assert(0);
    }
}

void
assign_image_userval_drawable (userval_info_t *info, userval_t *val, input_drawable_t *drawable)
{
    g_assert(info->type == USERVAL_IMAGE);

    if (val->v.image != NULL)
    {
	g_assert(val->v.image->type == IMAGE_DRAWABLE);

	if (val->v.image->v.drawable != NULL)
	    free_input_drawable(val->v.image->v.drawable);
    }

    val->v.image = &drawable->image;

    calc_image_values(info, val);
}

const char*
userval_type_name (int type)
{
    switch (type)
    {
	case USERVAL_INT_CONST : return "int";
	case USERVAL_FLOAT_CONST : return "float";
	case USERVAL_BOOL_CONST : return "bool";
	case USERVAL_COLOR : return "color";
	case USERVAL_CURVE : return "curve";
	case USERVAL_GRADIENT : return "gradient";
	case USERVAL_IMAGE : return "image";
	default : g_assert_not_reached();
    }
}
