/*
 * userval.c
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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#ifndef OPENSTEP
#include <gtk/gtk.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#endif

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
    g_assert(val->v.image.drawable);

    width = val->v.image.drawable->width;
    height = val->v.image.drawable->height;

    calc_scale_factors(info->v.image.flags, width, height,
		       &val->v.image.scale_x, &val->v.image.scale_y);
    calc_middle_values(width, height, 
		       1.0 / val->v.image.scale_x, 1.0 / val->v.image.scale_y,
		       &val->v.image.middle_x, &val->v.image.middle_y);
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
	    {
		int i;

		for (i = 0; i < USER_CURVE_POINTS; ++i)
		    val->v.curve.values[i] = (float)i / (float)(USER_CURVE_POINTS - 1);
	    }
	    break;

	case USERVAL_GRADIENT :
	    {
		int i;

		for (i = 0; i < USER_GRADIENT_POINTS; ++i)
		{
#ifndef OPENSTEP
		    val->v.gradient.values[i] = gradient_samples[i];
#else
		    unsigned char v = i * 255 / USER_GRADIENT_POINTS;

		    val->v.gradient.values[i] = MAKE_RGBA_COLOR(v,v,v,255);
#endif
		}
	    }
	    break;

	case USERVAL_COLOR :
#ifndef OPENSTEP
	    val->v.color.button_value.r =
		val->v.color.button_value.g =
		val->v.color.button_value.b = 0.0;
	    val->v.color.button_value.a = 1.0;
#endif

	    val->v.color.value = COLOR_BLACK;
	    break;

	case USERVAL_IMAGE :
	    val->v.image.drawable = 0;
	    /*
	    get_default_input_drawable();
	    if (val->v.image.drawable != 0)
		calc_image_values(info, val);
	    */
	    break;
    }
}

void
instantiate_userval (userval_t *val, userval_info_t *info, mathmap_invocation_t *invocation)
{
    val->type = info->type;

    switch (info->type)
    {
	case USERVAL_CURVE :
	    val->v.curve.values = (float*)malloc(USER_CURVE_POINTS * sizeof(float));
	    break;

	case USERVAL_GRADIENT :
	    val->v.gradient.values = (color_t*)malloc(USER_GRADIENT_POINTS * sizeof(color_t));
	    break;
    }

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
		free(uservals[info->index].v.curve.values);
		break;

	    case USERVAL_GRADIENT :
		free(uservals[info->index].v.gradient.values);
		break;

	    case USERVAL_IMAGE :
		if (uservals[info->index].v.image.drawable != 0)
		    free_input_drawable(uservals[info->index].v.image.drawable);
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

	case USERVAL_IMAGE :
	    {
		input_drawable_t *dst_drawable = dst->v.image.drawable;

		if (src->v.image.drawable != 0)
		    dst->v.image.drawable = copy_input_drawable(src->v.image.drawable);
		else
		    dst->v.image.drawable = 0;

		if (dst_drawable != 0)
		    free_input_drawable(dst_drawable);

		dst->v.image.scale_x = src->v.image.scale_x;
		dst->v.image.scale_y = src->v.image.scale_y;
		dst->v.image.middle_x = src->v.image.middle_x;
		dst->v.image.middle_y = src->v.image.middle_y;
	    }
	    break;

	case USERVAL_CURVE :
	    memcpy(dst->v.curve.values, src->v.curve.values, USER_CURVE_POINTS * sizeof(float));
	    break;

	case USERVAL_GRADIENT :
	    memcpy(dst->v.gradient.values, src->v.gradient.values, USER_GRADIENT_POINTS * sizeof(color_t));
	    break;

	default :
	    assert(0);
    }
}

void
assign_image_userval_drawable (userval_info_t *info, userval_t *val, input_drawable_t *drawable)
{
    assert(info->type == USERVAL_IMAGE);
    assert(val->type == USERVAL_IMAGE);

    if (val->v.image.drawable != 0)
	free_input_drawable(val->v.image.drawable);

    val->v.image.drawable = drawable;

    calc_image_values(info, val);
}

#ifndef OPENSTEP
static void
userval_int_update (GtkAdjustment *adjustment, userval_t *userval)
{
    userval->v.int_const = adjustment->value;

    user_value_changed();
}

static void
userval_float_update (GtkAdjustment *adjustment, userval_t *userval)
{
    userval->v.float_const = adjustment->value;

    user_value_changed();
}

static void
userval_bool_update (GtkToggleButton *button, userval_t *userval)
{
    if (gtk_toggle_button_get_active(button))
	userval->v.bool_const = 1.0;
    else
	userval->v.bool_const = 0.0;

    user_value_changed();
}

static void
userval_color_update (GtkWidget *color_well, userval_t *userval)
{
    gimp_color_button_get_color(GIMP_COLOR_BUTTON(color_well), &userval->v.color.button_value);
    userval->v.color.value = MAKE_RGBA_COLOR_FLOAT(userval->v.color.button_value.r,
						   userval->v.color.button_value.g,
						   userval->v.color.button_value.b,
						   userval->v.color.button_value.a);

    user_value_changed();
}

static void
user_image_update (gint32 id, void **user_data)
{
    userval_info_t *info = (userval_info_t*)user_data[0];
    userval_t *userval = (userval_t*)user_data[1];
    GimpDrawable *drawable;

    if (userval->v.image.drawable != 0
	&& get_gimp_input_drawable(userval->v.image.drawable) == gimp_drawable_get(id))
	return;

    drawable = gimp_drawable_get(id);
    assert(drawable != 0);

    assign_image_userval_drawable(info, userval, alloc_gimp_input_drawable(drawable));

    user_value_changed();
}

static int
make_table_entry_for_userval (userval_info_t *info, int *have_input_image)
{
    if (info->type == USERVAL_GRADIENT)
	return 0;
    if (info->type == USERVAL_IMAGE)
    {
	if (*have_input_image)
	    return 1;
	*have_input_image = 1;
	return 0;
    }
    return 1;
}

GtkWidget*
make_userval_table (userval_info_t *infos, userval_t *uservals)
{
    int i;
    userval_info_t *info;
    GtkWidget *table;
    int have_input_image;

    have_input_image = 0;
    i = 0;
    for (info = infos; info != 0; info = info->next)
	if (make_table_entry_for_userval(info, &have_input_image))
	    ++i;

    if (i == 0)
	return 0;

    table = gtk_table_new(i, 2, FALSE);

    have_input_image = 0;
    i = 0;
    for (info = infos; info != 0; info = info->next)
    {
	GtkWidget *widget = 0, *label;
	GtkAttachOptions xoptions = GTK_FILL | GTK_EXPAND, yoptions = 0;

	if (!make_table_entry_for_userval(info, &have_input_image))
	    continue;

	label = gtk_label_new(info->name);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, i, i + 1, 0, 0, 0, 0);
	gtk_widget_show(label);

	switch (info->type)
	{
	    case USERVAL_INT_CONST :
		{
		    GtkObject *adjustment;

		    adjustment = gtk_adjustment_new(uservals[info->index].v.int_const,
						    info->v.int_const.min,
						    info->v.int_const.max,
						    1, 10, 0.0);
		    gtk_signal_connect(adjustment, "value_changed",
				       (GtkSignalFunc)userval_int_update,
				       &uservals[info->index]);
		    widget = gtk_hscale_new(GTK_ADJUSTMENT(adjustment));
		    gtk_scale_set_digits(GTK_SCALE(widget), 0);
		}
		break;

	    case USERVAL_FLOAT_CONST :
		{
		    GtkObject *adjustment;
		    float range = info->v.float_const.max - info->v.float_const.min;
		    int exponent, j;
		    float increment;

		    j = exponent = (int)(floor(log10(range)) - 3);
		    increment = 1.0;
		    while (j > 0)
		    {
			increment *= 10.0;
			--j;
		    }
		    while (j < 0)
		    {
			increment /= 10.0;
			++j;
		    }

		    adjustment = gtk_adjustment_new(uservals[info->index].v.float_const,
						    info->v.float_const.min,
						    info->v.float_const.max,
						    increment, increment * 10, 0.0);
		    gtk_signal_connect(adjustment, "value_changed",
				       (GtkSignalFunc)userval_float_update,
				       &uservals[info->index]);
		    widget = gtk_hscale_new(GTK_ADJUSTMENT(adjustment));
		    if (exponent < 0)
			gtk_scale_set_digits(GTK_SCALE(widget), -exponent);
		    else
			gtk_scale_set_digits(GTK_SCALE(widget), 0);
		}
		break;

	    case USERVAL_BOOL_CONST :
		widget = gtk_check_button_new();
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget),
					     uservals[info->index].v.bool_const != 0.0);
		gtk_signal_connect(GTK_OBJECT(widget), "toggled",
				   (GtkSignalFunc)userval_bool_update,
				   &uservals[info->index]);
		break;

	    case USERVAL_COLOR :
		widget = gimp_color_button_new(info->name, 32, 16, &uservals[info->index].v.color.button_value,
					       GIMP_COLOR_AREA_SMALL_CHECKS);
		gtk_signal_connect(GTK_OBJECT(widget), "color_changed",
				   (GtkSignalFunc)userval_color_update,
				   &uservals[info->index]);
		break;

	    case USERVAL_CURVE :
		{
		    gfloat vector[USER_CURVE_POINTS];
		    int j;

		    for (j = 0; j < USER_CURVE_POINTS; ++j)
			vector[j] = uservals[info->index].v.curve.values[j] * (USER_CURVE_POINTS - 1);

		    widget = gtk_gamma_curve_new();
		    gtk_curve_set_range(GTK_CURVE(GTK_GAMMA_CURVE(widget)->curve),
					0, USER_CURVE_POINTS - 1, 0, USER_CURVE_POINTS - 1);
		    gtk_curve_set_vector(GTK_CURVE(GTK_GAMMA_CURVE(widget)->curve),
					 USER_CURVE_POINTS, vector);
		    gtk_curve_set_range(GTK_CURVE(GTK_GAMMA_CURVE(widget)->curve), 0, 1, 0, 1);

		    yoptions = GTK_FILL | GTK_EXPAND;
		}
		break;

	    case USERVAL_IMAGE :
		{
		    GtkWidget *menu;
		    gint32 drawable_id = -1;
		    void **user_data;

		    /* FIXME: extremely ugly hack - memory will never be reclaimed! */
		    user_data = (void**)malloc(sizeof(void*) * 2);
		    assert(user_data != 0);
		    user_data[0] = info;
		    user_data[1] = &uservals[info->index];

		    if (uservals[info->index].v.image.drawable != 0)
		    {
			GimpDrawable *drawable = get_gimp_input_drawable(uservals[info->index].v.image.drawable);

			if (drawable != 0)
			    drawable_id = GIMP_DRAWABLE_ID(drawable);
		    }

		    widget = gtk_option_menu_new();
		    menu = gimp_drawable_menu_new(NULL, (GimpMenuCallback)user_image_update,
						  user_data, drawable_id);
		    gtk_option_menu_set_menu(GTK_OPTION_MENU(widget), menu);
		}
		break;

	    case USERVAL_GRADIENT :
		assert(0);

	    default :
		assert(0);
	}

	if (widget != 0)
	{
	    gtk_table_attach(GTK_TABLE(table), widget, 1, 2, i, i + 1, xoptions, yoptions, 0, 0);
	    gtk_widget_show(widget);
	}

	uservals[info->index].widget = widget;

	++i;
    }

    gtk_widget_show(table);

    return table;
}

void
update_uservals (userval_info_t *infos, userval_t *uservals)
{
    userval_info_t *info;

    for (info = infos; info != 0; info = info->next)
	if (info->type == USERVAL_CURVE)
	    gtk_curve_get_vector(GTK_CURVE(GTK_GAMMA_CURVE(uservals[info->index].widget)->curve),
				 USER_CURVE_POINTS,
				 uservals[info->index].v.curve.values);
}
#endif
