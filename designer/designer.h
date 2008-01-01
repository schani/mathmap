/* -*- c -*- */

/*
 * designer.h
 *
 * MathMap
 *
 * Copyright (C) 2007 Mark Probst
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

#ifndef __DESIGNER_H__
#define __DESIGNER_H__

#include <glib.h>

typedef struct
{
    char *name;
} designer_type_t;

typedef struct
{
    char *name;
    designer_type_t *type;
} designer_slot_type_t;

typedef struct
{
    char *name;
    GSList *input_slot_types;
    GSList *output_slot_types;
} designer_node_type_t;

typedef struct _designer_node_t designer_node_t;

typedef struct
{
    struct _designer_node_t *partner; /* NULL if not assigned */
    int partner_slot_index;
} designer_slot_t;

struct _designer_node_t
{
    designer_node_type_t *type;
    char *name;
    designer_slot_t *input_slots;
    designer_slot_t *output_slots;
};

typedef struct
{
    gboolean allow_cycles;
    GSList *types;
    GSList *node_types;
} designer_design_type_t;

typedef struct
{
    designer_design_type_t *type;
    GSList *nodes;
} designer_design_t;

extern gboolean designer_verify_design (designer_design_t *design);
extern gboolean designer_design_contains_cycles (designer_design_t *design);

#endif
