/*
 * colorwell.h
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

#ifndef __COLORWELL_H__
#define __COLORWELL_H__

#include <gtk/gtk.h>

#define COLOR_WELL(obj)              GTK_CHECK_CAST(obj, color_well_get_type(), ColorWell)
#define COLOR_WELL_CLASS(klass)      GTK_CHECK_CLASS_CAST(klass, color_well_get_type(), ColorWellClass)
#define IS_COLOR_WELL(obj)           GTK_CHECK_TYPE(obj, color_well_get_type())

typedef struct _ColorWell ColorWell;
typedef struct _ColorWellClass ColorWellClass;

struct _ColorWell
{
    GtkVBox vbox;

    GtkPreview *preview;
    GtkColorSelectionDialog *color_selection_dialog;
    gdouble color[4];
};

struct _ColorWellClass
{
    GtkVBoxClass parent_class;

    void (*color_changed) (ColorWell *color_well);
};

guint color_well_get_type (void);
GtkWidget* color_well_new (void);

void color_well_set_color (ColorWell *color_well, gdouble *color);
void color_well_get_color (ColorWell *color_well, gdouble *color);

#endif
