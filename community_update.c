/* -*- c -*- */

/*
 * community_update.c
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

#include "community_update.h"
#include <couchdb-glib.h>
#include <assert.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "mathmap.h"
#include "communicator.h"

static void show_dialog(GtkWidget *parent);
static void doc_list_fetched(char *doc_id, GSList *docs, GError *error);
static void doc_fetched(char *doc_id, CouchDBDocument *doc, GError *error);
static void fetch_next_doc();
static void save_doc(CouchDBDocument *doc);

GtkWidget *dialog;
GtkWidget *status_label;
GtkWidget *progress;

GtkWidget *ok;
GtkWidget *cancel;

GSList *all_doc_infos;
GSList *doc_infos_ptr;
int fetch_doc_count;
int fetch_doc_cur;

void community_update(GtkWidget *parent) {
	communicator_list_documents((communicator_callback_t)doc_list_fetched, NULL);
	show_dialog(parent);
}

static void dialog_set_error(char *error) {
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progress), 1.0);
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progress), _("Error"));
	char *msg = g_strdup_printf("%s: %s", _("Error"), error);
	gtk_label_set_text(GTK_LABEL(status_label), msg);
	g_free(msg);

	gtk_widget_set_sensitive(ok, TRUE);
	gtk_widget_set_sensitive(cancel, FALSE);
	gtk_window_set_default(GTK_WINDOW(dialog), ok);
}

static void doc_list_fetched(char *doc_id, GSList *docs, GError *error) {
	all_doc_infos = docs;
	fetch_doc_count = g_slist_length(docs);
	fetch_doc_cur = 0;
	char *path_local;

	struct stat st;

	if (! dialog)
		return;

	if (error) {
		dialog_set_error(error->message);
		return;
	}

	all_doc_infos = docs;
	doc_infos_ptr = docs;

	gtk_label_set_text(GTK_LABEL(status_label), "Fetching expressions...");

	// ensure community local folder exists
	path_local = get_rc_file_name(EXPRESSIONS_COMMUNITY_DIR, 0);

	if (stat(path_local, &st)) {
		if (mkdir(path_local, 0755)) {
			printf("Could not create directory %s\n", path_local);
		}
	}


	fetch_next_doc();
}

static void fetch_next_doc() {
	gdouble fraction;
	char progress_text[256];

	fetch_doc_cur++;


	if (doc_infos_ptr) { // fetching next doc
		sprintf(progress_text, "%d %s %d", fetch_doc_cur, _("of"), fetch_doc_count);
		fraction = (gdouble)fetch_doc_cur / (gdouble)fetch_doc_count;
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progress), fraction);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progress), progress_text);

		CouchDBDocumentInfo *doc_info = (CouchDBDocumentInfo *)doc_infos_ptr->data;
		char *doc_id = (char *)couchdb_document_info_get_docid(doc_info);
		doc_infos_ptr = g_slist_next(doc_infos_ptr);
    	communicator_fetch_document(doc_id, (communicator_callback_t)doc_fetched, doc_id);

	} else { // fetched all docs
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progress), 1.0);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progress), _("Done"));
		gtk_label_set_text(GTK_LABEL(status_label), "Update finished");

		gtk_widget_set_sensitive(ok, TRUE);
		gtk_widget_set_sensitive(cancel, FALSE);
		gtk_window_set_default(GTK_WINDOW(dialog), ok);
	}
}



static void doc_fetched(char *doc_id, CouchDBDocument *doc, GError *error) {
	if (! dialog)
		return;
	if (error) {
		dialog_set_error(error->message);
		return;
	}

	save_doc(doc);
	// sleep(1);

	fetch_next_doc();
}

static void save_doc(CouchDBDocument *doc) {
	char *name;
	char *content;
	char *path_local;
	gchar *file_path;
	int type;
	char *ext;
	FILE *out;

	name = (char *)couchdb_document_get_id(doc);
	content = (char *)couchdb_document_get_string_field(doc, "content");
	path_local = get_rc_file_name(EXPRESSIONS_COMMUNITY_DIR, 0);

	// TODO: rename to kind!
	type = couchdb_document_get_int_field(doc, "type");
	switch (type) {
		case EXPRESSION_DB_EXPRESSION:
			ext = "mm";
			break;
		case EXPRESSION_DB_DESIGN:
			ext = "mmc";
			break;
		default:
			assert(0);
		// TODO: user value set
	}

	file_path = g_strdup_printf("%s/%s.%s", path_local, name, ext);

	// printf("file path: %s\n", file_path);

	out = fopen(file_path, "wb");
	if (out) {
		fwrite(content, sizeof(char), strlen(content), out);
		fclose(out);
	} else {
		printf("Could not write to file %s\n", file_path);
	}

	g_free(file_path);
}

static void show_dialog(GtkWidget *parent)
{
	guint response;
	dialog = gtk_dialog_new_with_buttons (
		_("Community Update"),
		GTK_WINDOW(parent), //get_root_window(parent),
		GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
		/*
		GTK_STOCK_OK,
		GTK_RESPONSE_OK,
		GTK_STOCK_CANCEL,
		GTK_RESPONSE_CANCEL,
		*/
		NULL
	);
	//gtk_widget_set_size_request(dialog, 300, 200);

	ok = gtk_dialog_add_button(GTK_DIALOG(dialog), GTK_STOCK_OK, GTK_RESPONSE_OK);
	cancel = gtk_dialog_add_button(GTK_DIALOG(dialog), GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
	gtk_widget_set_sensitive(ok, FALSE);
	gtk_window_set_default(GTK_WINDOW(dialog), cancel);

	status_label = gtk_label_new(_("Fetching expressions list..."));
	progress = gtk_progress_bar_new();

	gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), status_label);
	gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), progress);

	gtk_widget_show_all(dialog);
	response = gtk_dialog_run(GTK_DIALOG(dialog));
	switch (response) {
		case GTK_RESPONSE_OK :
			break;

		default :
			break;
	}

	gtk_widget_destroy(dialog);
}

