/* -*- c -*- */

/*
 * expression_panel.h
 *
 * MathMap
 *
 * Copyright (C) 2010 Genadz Batsyan
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

#ifndef __EXPRESSION_PANEL_H__
#define __EXPRESSION_PANEL_H__

#include <glib.h>
#include <gtk/gtk.h>

#include "expression_db.h"

typedef void (*expression_panel_callback_t) (GtkWidget *widget);

GtkWidget *expression_panel_new(expression_panel_callback_t callback);
void expression_panel_set_edb(GtkWidget *widget, expression_db_t *edb);
void expression_panel_refresh(GtkWidget *widget);
expression_db_t *expression_panel_get_selected_expression(GtkWidget *widget);

// needed in mathmap.c
extern void expression_comment_callback(char *comment);
extern expression_metadata_t *cur_meta;

expression_metadata_t *expression_metadata_new();
expression_metadata_t *expression_metadata_copy(expression_metadata_t *meta);
void expression_metadata_free(expression_metadata_t *meta);

#endif

