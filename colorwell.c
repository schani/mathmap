/*
 * colorwell.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2000 Mark Probst
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

#include "colorwell.h"

enum {
    COLOR_CHANGED_SIGNAL,
    LAST_SIGNAL
};

static guint color_well_signals[LAST_SIGNAL] = { 0 };

static void
color_well_class_init (ColorWellClass *class)
{
    GtkObjectClass *object_class;

    object_class = (GtkObjectClass*)class;

    color_well_signals[COLOR_CHANGED_SIGNAL] = gtk_signal_new("color-changed",
							      GTK_RUN_FIRST,
							      object_class->type,
							      GTK_SIGNAL_OFFSET(ColorWellClass, color_changed),
							      gtk_signal_default_marshaller, GTK_TYPE_NONE, 0);

    gtk_object_class_add_signals(object_class, color_well_signals, LAST_SIGNAL);

    class->color_changed = NULL;
}

static void
color_well_update_color (ColorWell *color_well)
{
    guchar buf[32 * 3];
    int i;

    for (i = 0; i < 32; ++i)
    {
	buf[3 * i + 0] = color_well->color[0] * 255.0;
	buf[3 * i + 1] = color_well->color[1] * 255.0;
	buf[3 * i + 2] = color_well->color[2] * 255.0;
    }

    for (i = 0; i < 16; ++i)
	gtk_preview_draw_row(color_well->preview, buf, 0, i, 32);
    gtk_widget_draw(GTK_WIDGET(color_well->preview), 0);
}

static void
color_well_cancel_callback (GtkWidget *widget, ColorWell *color_well)
{
    gtk_widget_destroy(GTK_WIDGET(color_well->color_selection_dialog));
    color_well->color_selection_dialog = 0;
}

static void
color_well_ok_callback (GtkWidget *widget, ColorWell *color_well)
{
    gtk_color_selection_get_color(GTK_COLOR_SELECTION(color_well->color_selection_dialog->colorsel), color_well->color);

    gtk_widget_destroy(GTK_WIDGET(color_well->color_selection_dialog));
    color_well->color_selection_dialog = 0;

    color_well_update_color(color_well);

    gtk_signal_emit(GTK_OBJECT(color_well), color_well_signals[COLOR_CHANGED_SIGNAL]);
}

static void
color_well_button_clicked (GtkWidget *button, ColorWell *color_well)
{
    GtkWidget *dialog;

    if (color_well->color_selection_dialog != 0)
	return;

    dialog = gtk_color_selection_dialog_new("Edge Color");
    color_well->color_selection_dialog = GTK_COLOR_SELECTION_DIALOG(dialog);
    gtk_color_selection_set_opacity(GTK_COLOR_SELECTION(color_well->color_selection_dialog->colorsel), 1);

    gtk_widget_destroy(GTK_COLOR_SELECTION_DIALOG(dialog)->help_button);
    gtk_signal_connect(GTK_OBJECT(dialog), "destroy",
		       (GtkSignalFunc)color_well_cancel_callback, color_well);
    gtk_signal_connect(GTK_OBJECT(GTK_COLOR_SELECTION_DIALOG(dialog)->ok_button), "clicked",
		       (GtkSignalFunc)color_well_ok_callback, color_well);
    gtk_signal_connect(GTK_OBJECT(GTK_COLOR_SELECTION_DIALOG(dialog)->cancel_button), "clicked",
		       (GtkSignalFunc)color_well_cancel_callback, color_well);

    gtk_color_selection_set_color(GTK_COLOR_SELECTION(GTK_COLOR_SELECTION_DIALOG(dialog)->colorsel), color_well->color);

    gtk_window_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);
    gtk_widget_show(dialog);
}

static void
color_well_init (ColorWell *color_well)
{
    GtkWidget *button, *preview;

    button = gtk_button_new();
    preview = gtk_preview_new(GTK_PREVIEW_COLOR);
    color_well->preview = GTK_PREVIEW(preview);
    gtk_preview_size(GTK_PREVIEW(preview), 32, 16);
    gtk_container_add(GTK_CONTAINER(button), preview);
    gtk_widget_show(preview);
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       (GtkSignalFunc)color_well_button_clicked, color_well);
    gtk_widget_show(button);

    gtk_container_add(GTK_CONTAINER(color_well), button);

    color_well->color_selection_dialog = 0;

    color_well->color[0] = color_well->color[1] = color_well->color[2] = 0.0;
    color_well->color[3] = 1.0;

    color_well_update_color(color_well);
}

guint
color_well_get_type (void)
{
    static guint type = 0;

    if (type == 0)
    {
	GtkTypeInfo info =
	{
	    "ColorWell",
	    sizeof(ColorWell),
	    sizeof(ColorWellClass),
	    (GtkClassInitFunc)color_well_class_init,
	    (GtkObjectInitFunc)color_well_init,
	    (GtkArgSetFunc)NULL,
	    (GtkArgGetFunc)NULL
	};

	type = gtk_type_unique(gtk_vbox_get_type(), &info);
    }

    return type;
}

GtkWidget*
color_well_new (void)
{
    return GTK_WIDGET(gtk_type_new(color_well_get_type()));
}

void
color_well_set_color (ColorWell *color_well, gdouble *color)
{
    int i;

    for (i = 0; i < 4; ++i)
	color_well->color[i] = color[i];

    color_well_update_color(color_well);
}

void
color_well_get_color (ColorWell *color_well, gdouble *color)
{
    int i;

    for (i = 0; i < 4; ++i)
	color[i] = color_well->color[i];
}
