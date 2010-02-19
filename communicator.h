/* -*- c -*- */

/*
 * communicator.h
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

#ifndef __COMMUNICATOR_H__
#define __COMMUNICATOR_H__

#include <glib.h>

typedef void (*communicator_callback_t) (gpointer data, gpointer result);

// result: GSList of CouchDBDocumentInfo*
void communicator_list_documents(communicator_callback_t callback, gpointer data);

// result: CouchDBDocument*
void communicator_fetch_document(char *doc_id, communicator_callback_t callback, gpointer data);

#endif

