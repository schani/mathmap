#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include <gtk/gtk.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

#include "mathmap.h"
#include "userval.h"
#include "tags.h"
#include "colorwell.h"

static userval_t *first = 0;

static userval_t*
alloc_and_register_userval (const char *name, int type)
{
    userval_t *userval = (userval_t*)malloc(sizeof(userval_t));
    userval_t **p;

    strncpy(userval->name, name, MAX_IDENT_LENGTH);
    userval->name[MAX_IDENT_LENGTH] = '\0';

    userval->type = type;

    userval->tag = 1;

    userval->next = 0;

    p = &first;
    while (*p != 0)
	p = &(*p)->next;
    *p = userval;

    return userval;
}

void
untag_uservals (void)
{
    userval_t *userval;

    for (userval = first; userval != 0; userval = userval->next)
	userval->tag = 0;
}

void
clear_untagged_uservals (void)
{
    userval_t **userval;

    userval = &first;
    while (*userval != 0)
    {
	if (!(*userval)->tag)
	{
	    userval_t *p = *userval;

	    *userval = (*userval)->next;

	    if (p->type == USERVAL_IMAGE && p->v.image.index != -1)
		free_input_drawable(p->v.image.index);

	    free(p);
	}
	else
	    userval = &(*userval)->next;
    }
}

userval_t*
lookup_userval (const char *name, int type)
{
    userval_t *userval;

    for (userval = first; userval != 0; userval = userval->next)
	if (userval->type == type && strcmp(name, userval->name) == 0)
	    return userval;

    return 0;
}

userval_t*
register_slider (const char *name, float min, float max)
{
    userval_t *userval;

    userval = lookup_userval(name, USERVAL_SLIDER);
    if (userval != 0 && userval->tag)
    {
	if (userval->v.slider.min == min && userval->v.slider.max == max)
	    return userval;
	return 0;
    }
    else if (userval != 0 && !userval->tag)
    {
	if (userval->v.slider.min != min || userval->v.slider.max != max)
	{
	    userval->v.slider.min = min;
	    userval->v.slider.max = max;
	    userval->v.slider.value = min;
	}
	userval->tag = 1;
    }
    else
    {
	userval = alloc_and_register_userval(name, USERVAL_SLIDER);
	userval->v.slider.min = min;
	userval->v.slider.max = max;
	userval->v.slider.value = min;
    }

    return userval;
}

userval_t*
register_bool (const char *name)
{
    userval_t *userval;

    userval = lookup_userval(name, USERVAL_BOOL);
    if (userval != 0)
    {
	userval->tag = 1;

	return userval;
    }

    userval = alloc_and_register_userval(name, USERVAL_BOOL);

    return userval;
}

userval_t*
register_color (const char *name)
{
    userval_t *userval;

    userval = lookup_userval(name, USERVAL_COLOR);
    if (userval != 0)
    {
	userval->tag = 1;

	return userval;
    }

    userval = alloc_and_register_userval(name, USERVAL_COLOR);

    userval->v.color.value.number = rgba_tag_number;
    userval->v.color.value.length = 4;
    userval->v.color.value.data[0] = userval->v.color.value.data[1] = userval->v.color.value.data[2] = 0.0;
    userval->v.color.value.data[3] = 1.0;

    return userval;
}

userval_t*
register_curve (const char *name)
{
    userval_t *userval;
    int i;

    userval = lookup_userval(name, USERVAL_CURVE);
    if (userval != 0)
    {
	userval->tag = 1;

	return userval;
    }

    userval = alloc_and_register_userval(name, USERVAL_CURVE);

    for (i = 0; i < USER_CURVE_POINTS; ++i)
	userval->v.curve.values[i] = (float)i / (float)(USER_CURVE_POINTS - 1);

    return userval;
}

userval_t*
register_image (const char *name)
{
    userval_t *userval;

    userval = lookup_userval(name, USERVAL_IMAGE);
    if (userval != 0)
    {
	userval->tag = 1;

	return userval;
    }

    userval = alloc_and_register_userval(name, USERVAL_IMAGE);

    userval->v.image.index = -1;

    return userval;
}

static void
userval_slider_update (GtkAdjustment *adjustment, userval_t *userval)
{
    userval->v.slider.value = adjustment->value;

    if (auto_preview)
	dialog_update_preview();
}

static void
userval_bool_update (GtkToggleButton *button, userval_t *userval)
{
    if (gtk_toggle_button_get_active(button))
	userval->v.bool.value = 1.0;
    else
	userval->v.bool.value = 0.0;

    if (auto_preview)
	dialog_update_preview();
}

static void
userval_color_update (ColorWell *color_well, userval_t *userval)
{
    gdouble color[4];
    int i;

    color_well_get_color(color_well, color);
    for (i = 0; i < 4; ++i)
	userval->v.color.value.data[i] = color[i];

    if (auto_preview)
	dialog_update_preview();
}

static gint
user_image_constrain (gint32 image_id, gint32 drawable_id, gpointer data)
{
    if (drawable_id == -1)
	return TRUE;

    if (gimp_drawable_width(drawable_id) == img_width
	&& gimp_drawable_height(drawable_id) == img_height)
	return TRUE;
    else
	return FALSE;
}

static void
user_image_update (gint32 id, userval_t *userval)
{
    if (userval->v.image.index != -1)
	free_input_drawable(userval->v.image.index);
    userval->v.image.index = alloc_input_drawable(gimp_drawable_get(id));
}

GtkWidget*
make_userval_table (void)
{
    int i;
    userval_t *userval;
    GtkWidget *table;

    i = 0;
    for (userval = first; userval != 0; userval = userval->next)
	++i;

    if (i == 0)
	return 0;

    table = gtk_table_new(i, 2, FALSE);

    i = 0;
    for (userval = first; userval != 0; userval = userval->next)
    {
	GtkWidget *widget = 0, *label;
	GtkAttachOptions xoptions = GTK_FILL | GTK_EXPAND, yoptions = 0;

	label = gtk_label_new(userval->name);
	gtk_widget_show(label);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, i, i + 1, 0, 0, 0, 0);

	if (userval->type == USERVAL_SLIDER)
	{
	    GtkObject *adjustment;
	    float range = userval->v.slider.max - userval->v.slider.min;
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

	    adjustment = gtk_adjustment_new(userval->v.slider.value,
					    userval->v.slider.min,
					    userval->v.slider.max,
					    increment, increment * 10, 0.0);
	    gtk_signal_connect(adjustment, "value_changed",
			       (GtkSignalFunc)userval_slider_update,
			       userval);
	    widget = gtk_hscale_new(GTK_ADJUSTMENT(adjustment));
	    if (exponent < 0)
		gtk_scale_set_digits(GTK_SCALE(widget), -exponent);
	    else
		gtk_scale_set_digits(GTK_SCALE(widget), 0);

	    gtk_widget_show(widget);
	}
	else if (userval->type == USERVAL_BOOL)
	{
	    widget = gtk_check_button_new();
	    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget),
					 userval->v.bool.value != 0.0);
	    gtk_signal_connect(GTK_OBJECT(widget), "toggled",
			       (GtkSignalFunc)userval_bool_update,
			       userval);
	    gtk_widget_show(widget);
	}
	else if (userval->type == USERVAL_COLOR)
	{
	    gdouble color[4];
	    int j;

	    widget = color_well_new();
	    for (j = 0; j < 4; ++j)
		color[j] = userval->v.color.value.data[j];
	    color_well_set_color(COLOR_WELL(widget), color);
	    gtk_signal_connect(GTK_OBJECT(widget), "color-changed",
			       (GtkSignalFunc)userval_color_update,
			       userval);
	    gtk_widget_show(widget);
	}
	else if (userval->type == USERVAL_CURVE)
	{
	    gfloat vector[USER_CURVE_POINTS];
	    int j;

	    for (j = 0; j < USER_CURVE_POINTS; ++j)
		vector[j] = userval->v.curve.values[j] * (USER_CURVE_POINTS - 1);

	    widget = gtk_gamma_curve_new();
	    gtk_widget_show(widget);
	    gtk_curve_set_range(GTK_CURVE(GTK_GAMMA_CURVE(widget)->curve),
				0, USER_CURVE_POINTS - 1, 0, USER_CURVE_POINTS - 1);
	    gtk_curve_set_vector(GTK_CURVE(GTK_GAMMA_CURVE(widget)->curve),
				 USER_CURVE_POINTS, vector);
	    gtk_curve_set_range(GTK_CURVE(GTK_GAMMA_CURVE(widget)->curve), 0, 1, 0, 1);

	    yoptions = GTK_FILL | GTK_EXPAND;
	}
	else if (userval->type == USERVAL_IMAGE)
	{
	    GtkWidget *menu;
	    gint32 drawable_id = -1;

	    if (userval->v.image.index != -1)
	    {
		GDrawable *drawable = get_input_drawable(userval->v.image.index);

		if (drawable != 0)
		    drawable_id = drawable->id;
	    }

	    widget = gtk_option_menu_new();
	    menu = gimp_drawable_menu_new(user_image_constrain, (GimpMenuCallback)user_image_update,
					  userval, drawable_id);
	    gtk_option_menu_set_menu(GTK_OPTION_MENU(widget), menu);
	    gtk_widget_show(widget);
	}
	else
	    assert(0);

	gtk_table_attach(GTK_TABLE(table), widget, 1, 2, i, i + 1, xoptions, yoptions, 0, 0);

	userval->widget = widget;

	++i;
    }

    gtk_widget_show(table);

    return table;
}

void
update_uservals (void)
{
    userval_t *userval;

    for (userval = first; userval != 0; userval = userval->next)
    {
	if (userval->type == USERVAL_CURVE)
	    gtk_curve_get_vector(GTK_CURVE(GTK_GAMMA_CURVE(userval->widget)->curve), USER_CURVE_POINTS, userval->v.curve.values);
    }
}
