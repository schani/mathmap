/* -*- c -*- */

/*
 * expression_panel.c
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

#include <assert.h>
#include <string.h>
#include <glib.h>
#include <gtk/gtk.h>
#include "expression_panel.h"

#include "mathmap.h"
#include "scanner.h"

#define TREE_VALUE_NAME	0
#define TREE_VALUE_EDB	1

typedef struct
{
	GtkWidget *container;
	GtkWidget *tags;
	GtkWidget *tags_scroller;
	GtkWidget *expressions;
	GtkWidget *expressions_scroller;
	expression_db_t *edb;
	expression_db_t *selected_expression;
	expression_panel_callback_t callback; // on expression selection change
} expression_panel_data_t;

typedef void (*edb_callback_t) (expression_db_t *edb);

// used as temporary var to filter visible expressions
static GList *selected_tags = NULL;

static expression_panel_data_t *get_widget_data(GtkWidget *widget);
static void refresh_expressions(expression_panel_data_t *data);
static GtkWidget *make_tags_widget(expression_panel_data_t *data);
static GtkWidget *make_expressions_widget(expression_panel_data_t *data);
static GtkWidget* make_scrolled_window();

static void populate_tags_store(GtkListStore *store, GtkTreeIter *parent, expression_db_t *edb);
static void populate_expressions_store(GtkTreeStore *store, GtkTreeIter *parent, expression_db_t *edb);
static GList *find_expressions_matching_selected_tags(expression_db_t *edb);


// TODO: move to mathmap.h

static gboolean tags_selection_changed_event (GtkTreeSelection *selection, gpointer user_data);
static gboolean expressions_selection_changed_event (GtkTreeSelection *selection, gpointer user_data);


GtkWidget *expression_panel_new(expression_panel_callback_t callback) {
	expression_panel_data_t *data;

	data = g_new0(expression_panel_data_t, 1);
	data->callback = callback;

	data->container = gtk_hpaned_new();

	data->tags_scroller = make_scrolled_window();
	gtk_widget_show(data->tags_scroller);

	data->expressions_scroller = make_scrolled_window();
	gtk_widget_show(data->expressions_scroller);

	gtk_paned_add1(GTK_PANED(data->container), data->tags_scroller);
	gtk_paned_add2(GTK_PANED(data->container), data->expressions_scroller);

	gtk_widget_show(data->container);

	g_object_set_data(G_OBJECT(data->container), "panel-data", data);

	return data->container;
}

void expression_panel_set_edb(GtkWidget *widget, expression_db_t *edb) {
	expression_panel_data_t *data = get_widget_data(widget);
	data->edb = edb;
}

void expression_panel_refresh(GtkWidget *widget)
{
	expression_panel_data_t *data = get_widget_data(widget);

	if (gtk_bin_get_child(GTK_BIN(data->tags_scroller)) != 0)
		gtk_container_remove(GTK_CONTAINER(data->tags_scroller), gtk_bin_get_child(GTK_BIN(data->tags_scroller)));

	data->tags = make_tags_widget(data);

	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(data->tags_scroller), data->tags);
	gtk_widget_show(data->tags);

	refresh_expressions(data);
}

expression_db_t *expression_panel_get_selected_expression(GtkWidget *widget) {
	expression_panel_data_t *data = get_widget_data(widget);
	return data->selected_expression;
}

void expression_panel_destroy(GtkWidget *widget) {
}


// private functions

static expression_panel_data_t *get_widget_data(GtkWidget *widget) {
	expression_panel_data_t *data = g_object_get_data(G_OBJECT(widget), "panel-data");

	g_assert(data != NULL);
	return data;
}


// build expression list according to selected tags
static void refresh_expressions(expression_panel_data_t *data) {
	GtkWidget *expressions;

	if (gtk_bin_get_child(GTK_BIN(data->expressions_scroller)) != 0)
		gtk_container_remove(GTK_CONTAINER(data->expressions_scroller), gtk_bin_get_child(GTK_BIN(data->expressions_scroller)));

	expressions = make_expressions_widget(data);
	data->expressions = expressions;

	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(data->expressions_scroller), expressions);
	gtk_widget_show(expressions);
}

static GtkWidget *make_tags_widget(expression_panel_data_t *data) {
	GtkListStore *store;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;
	GtkWidget *tree;

	tree = gtk_tree_view_new();
	g_object_set_data(G_OBJECT(tree), "panel-data", data);


	/* model */
	store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_POINTER);

	// populate store
	if (data->edb)
		populate_tags_store(store, NULL, data->edb);

	/* view */
	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

	g_signal_connect(GTK_OBJECT(selection), "changed",
		(GtkSignalFunc)tags_selection_changed_event, data);

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes(_("Tags"), renderer,
		"text", 0,
		NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	return tree;
}

static GtkWidget *make_expressions_widget(expression_panel_data_t *data) {
	GtkTreeStore *store;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;
	GtkWidget *tree;

	tree = gtk_tree_view_new();
	g_object_set_data(G_OBJECT(tree), "panel-data", data);

	/* model */
	store = gtk_tree_store_new(2, G_TYPE_STRING, G_TYPE_POINTER);

	if (data->edb)
		populate_expressions_store(store, NULL, data->edb);

	/* view */
	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

	g_signal_connect(GTK_OBJECT(selection), "changed",
		(GtkSignalFunc)expressions_selection_changed_event, data);

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes(_("Filters"), renderer,
		"text", 0,
		NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	return tree;
}

static GtkWidget* make_scrolled_window() {
	GtkWidget *scrolled_window;

	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(
		GTK_SCROLLED_WINDOW(scrolled_window),
		GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC
	);

	return scrolled_window;
}


static void gather_selected_tags(
	GtkTreeModel *model,
	GtkTreePath *path,
	GtkTreeIter *iter,
	gpointer data) {

	GValue value = { 0, };
	char *tag;

	gtk_tree_model_get_value(model, iter, TREE_VALUE_NAME, &value);
	tag = (char *)g_value_get_string(&value);
	selected_tags = g_list_append(selected_tags, tag);
}

// event handlers
static gboolean tags_selection_changed_event (GtkTreeSelection *selection, gpointer user_data) {
	expression_panel_data_t *data = (expression_panel_data_t *)user_data;

	// TODO: free current selected_tags
	selected_tags = NULL;
	gtk_tree_selection_selected_foreach(selection, gather_selected_tags, NULL);

	refresh_expressions(data);

	return FALSE;
}
static gboolean expressions_selection_changed_event (GtkTreeSelection *selection, gpointer user_data) {
	GtkTreeModel *model;
	GtkTreeIter iter;
	expression_panel_data_t *data = (expression_panel_data_t *)user_data;

	if (selection && gtk_tree_selection_get_selected(selection, &model, &iter)) {
		GValue value = { 0, };
		gtk_tree_model_get_value(model, &iter, TREE_VALUE_EDB, &value);
		data->selected_expression = (expression_db_t *)g_value_get_pointer(&value);
	} else {
		data->selected_expression = NULL;
	}
	if (data->callback)
		data->callback(data->container);
	return FALSE;
}

static gint string_glist_comparator(gconstpointer a, gconstpointer b) {
	return strcasecmp((char *)a, (char *)b);
}
static gint expression_glist_comparator(gconstpointer a, gconstpointer b) {
	expression_db_t *ae = (expression_db_t *)a;
	expression_db_t *be = (expression_db_t *)b;

	return strcasecmp(ae->name, be->name);
}

GList *get_tag_list_from_edb(expression_db_t *edb) {
	GList *all_tags = NULL;
	gboolean have_tagless = TRUE;

	for (; edb != 0; edb = edb->next) {
		GList *all_tags_ptr;
		GList *tags_ptr;

		if (! edb->meta || ! edb->meta->tags) {
			have_tagless = TRUE;
			continue;
		}

		tags_ptr = edb->meta->tags;
		while (tags_ptr) {
			gboolean found = FALSE;
			all_tags_ptr = all_tags;
			while (all_tags_ptr && ! found) {
				if (! strcasecmp(all_tags_ptr->data, tags_ptr->data)) {
					found = TRUE;
					// printf("FOUND: %s\n", (char *)tags_ptr->data);
				}
				all_tags_ptr = g_list_next(all_tags_ptr);
			}
			if (! found) {
				all_tags = g_list_append(all_tags, g_strdup(tags_ptr->data));
			}
			tags_ptr = g_list_next(tags_ptr);
		}
	}

	all_tags = g_list_sort(all_tags, string_glist_comparator);
	if (have_tagless)
		all_tags = g_list_append(all_tags, g_strdup(NO_TAGS));
	return all_tags;
}
void deep_glist_free(GList *list) {
	GList *ptr = list;
	while (ptr) {
	    g_free(ptr->data);
	    ptr = g_list_next(ptr);
	}
	g_list_free(list);
}

static void populate_tags_store(GtkListStore *store, GtkTreeIter *parent, expression_db_t *edb) {
	GList *all_tags_ptr;

	GList *all_tags = get_tag_list_from_edb(edb);

	all_tags_ptr = all_tags;
	while (all_tags_ptr) {
		GtkTreeIter iter;
		// printf("ALL: %s\n", (char *)all_tags_ptr->data);
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
		       TREE_VALUE_NAME, (char *)all_tags_ptr->data,
		       TREE_VALUE_EDB, edb,
		       -1);
		all_tags_ptr = g_list_next(all_tags_ptr);
	}

	deep_glist_free(all_tags);
}

static void populate_expressions_store(GtkTreeStore *store, GtkTreeIter *parent, expression_db_t *edb) {
	GList *visible_expressions_ptr;

	GList *visible_expressions = find_expressions_matching_selected_tags(edb);

	visible_expressions = g_list_sort(visible_expressions, expression_glist_comparator);

	visible_expressions_ptr = visible_expressions;
	while (visible_expressions_ptr) {
		GtkTreeIter iter;
		expression_db_t *expr = (expression_db_t *)visible_expressions_ptr->data;
		gtk_tree_store_append(store, &iter, parent);
		gtk_tree_store_set(store, &iter,
		       TREE_VALUE_NAME, expr->name,
		       TREE_VALUE_EDB, expr,
		       -1);
		visible_expressions_ptr = g_list_next(visible_expressions_ptr);
	}
}
gboolean expression_has_tag(expression_db_t *expr, char *tag) {
	if (! expr->meta || ! expr->meta->tags) {
		if (strcasecmp(tag, NO_TAGS) == 0)
			return TRUE;
		else
			return FALSE;
	}
	GList *ptr = expr->meta->tags;
	while (ptr && strcasecmp(ptr->data, tag))
		ptr = g_list_next(ptr);
	return ptr ? TRUE : FALSE;
}
static GList *find_expressions_matching_selected_tags(expression_db_t *edb) {
	expression_db_t *expr;

	GList *result;

	for (expr = edb; expr; expr = expr->next) {
		if (! selected_tags) { // selecting all expressions
			result = g_list_append(result, expr);
		} else {
			GList *ptr = selected_tags;

			while (ptr) {
				if (! expression_has_tag(expr, ptr->data))
					break;
				ptr = g_list_next(ptr);
			}
			if (! ptr)
				result = g_list_append(result, expr);
		}
	}

	return result;
}

/* expression metadata functions */
expression_metadata_t *expression_metadata_new() {
	return g_new0(expression_metadata_t, 1);
}
expression_metadata_t *expression_metadata_copy(expression_metadata_t *meta) {
	GList *tags;

	expression_metadata_t *copy = expression_metadata_new();
	if (meta->title)
		copy->title = g_strdup(meta->title);

	// copying tags
	tags = meta->tags;
	while (tags) {
		copy->tags = g_list_append(copy->tags, g_strdup((char *)tags->data));
		tags = g_list_next(tags);
	}
	return copy;
}

void expression_metadata_free(expression_metadata_t *meta) {
return;
	GList *tags;

	if (meta->title)
		g_free(meta->title);
	tags = meta->tags;
	while (tags) {
		g_free(meta->tags->data);
		tags = g_list_next(tags);
	}
	if (meta->tags)
		g_list_free(meta->tags);
	g_free(meta);
}


expression_metadata_t *cur_meta;
void expression_comment_callback(char *comment) {
	GMatchInfo *matches;
	gchar *tag;
	gchar *value;

	static GRegex *meta_rex = NULL;
	static GRegex *comma_rex = NULL;
	if (! meta_rex) {
		meta_rex = g_regex_new("@([a-z]+)\\s+(.*\\S)", 0, 0, NULL);
		comma_rex = g_regex_new("\\s*,\\s*", 0, 0, NULL);
	}

	if (g_regex_match(meta_rex, comment, 0, &matches)) {
		// printf("COMMENT: %s\n", comment);
		tag = g_match_info_fetch(matches, 1);
		value = g_match_info_fetch(matches, 2);
		// printf("META: [%s]:[%s]\n", tag, value);
		if (!strcmp(tag, "title")) {
			cur_meta->title = g_strdup(value);
		} else if (!strcmp(tag, "tags")) {
			GList *list = NULL;
			gchar **ptr;
			gchar **parts = g_regex_split(comma_rex, value, 0);
			ptr = parts;
			while (*ptr) {
				// printf("TAG:[%s]\n", *ptr);
				list = g_list_append(list, g_strdup(*ptr));
				ptr++;
			}
			g_strfreev(parts);
			cur_meta->tags = list;
		} else {
			printf("Unrecognized metadata tag: %s\n", tag);
		}
	}

	free(comment);
}

