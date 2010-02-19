/* -*- c -*- */

/*
 * communicator.c
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

#include "communicator.h"

#include <couchdb-glib.h>
#include <stdio.h>

struct _communicator_task_t;
typedef void (*task_func_t) (struct _communicator_task_t *task);

typedef struct _communicator_task_t {
	communicator_callback_t callback; // will be called once the task is completed
	gpointer data; // passed as first parameter to the callback
	GError *error;
	task_func_t task_func;
	gpointer result;
	union {
		struct {
			gchar *doc_id;
		} fetch_document;
	} params;
} communicator_task_t;

static gboolean initialized = FALSE;
static CouchDB *db;
static char *dbname;

static void initialize() {
	if (! initialized) {
		if (!g_thread_supported())
			g_thread_init(NULL);
		db = couchdb_new("http://localhost:5984");
		dbname = "mathmap";
		initialized = TRUE;
	}
}

// create and initialize task structure
static communicator_task_t *task_new(task_func_t task_func, communicator_callback_t callback, gpointer data) {
	communicator_task_t *task = g_new0(communicator_task_t, 1);
	task->task_func = task_func;
	task->callback = callback;
	task->data = data;
	return task;
}

// executes in main thread after task is done
static gboolean source_func(gpointer data) {
	communicator_task_t *task = (communicator_task_t *)data;
	task->callback(task->data, task->result);
	g_free(task);
	return FALSE; // kill source after execution
}

// executes in task thread
static gpointer async_task_thread_func(gpointer data) {
	communicator_task_t *task = (communicator_task_t *)data;
	task->task_func(task);
	// guint source_id =
	(void)g_idle_add(source_func, task);
	return NULL;
}

// execute the task asynchronously
static void execute_task(communicator_task_t *task) {
	GError *error;
	initialize();
	// GThread *thread =
	(void)g_thread_create(async_task_thread_func, task, FALSE, &error);
	// TODO: free thread
	// TODO: implement task queue
}



/* Tasks functions */

// list documents
static void task_list_documents(communicator_task_t *task) {
	GSList *docs = couchdb_list_documents(db, dbname, &task->error);
	task->result = docs;
}
void communicator_list_documents(communicator_callback_t callback, gpointer data) {
	communicator_task_t *task = task_new(task_list_documents, callback, data);
	execute_task(task);
}

// fetch document by id
static void task_fetch_document(communicator_task_t *task) {
	CouchDBDocument *doc = couchdb_document_get(db, dbname, task->params.fetch_document.doc_id, &task->error);
	g_free(task->params.fetch_document.doc_id);
	task->result = doc;
}
void communicator_fetch_document(char *doc_id, communicator_callback_t callback, gpointer data) {
	communicator_task_t *task = task_new(task_fetch_document, callback, data);
	task->params.fetch_document.doc_id = g_strdup(doc_id);
	execute_task(task);
}

/*
	GSList *docs = couchdb_list_documents(db, dbname, &error);
	while (ptr) {
		CouchDBDocumentInfo *doc_info = (CouchDBDocumentInfo *)ptr->data;
		printf("docid: %s\n", couchdb_document_info_get_docid(doc_info));
		ptr = g_slist_next(ptr);
	}
*/



