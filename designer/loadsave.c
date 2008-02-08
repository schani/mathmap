/* -*- c -*- */

/*
 * loadsave.c
 *
 * MathMap
 *
 * Copyright (C) 2008 Mark Probst
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

#include "../lispreader/lispreader.h"

#include "designer.h"

designer_design_t*
designer_load_design (designer_design_type_t *design_type, const char *filename)
{
    return NULL;
}

gboolean
designer_save_design (designer_design_t *design, const char *filename)
{
    FILE *out = fopen(filename, "w");
    GSList *list;

    if (out == NULL)
	return FALSE;

    lisp_print_open_paren(out);
    lisp_print_symbol("design", out);

    for (list = design->nodes; list != NULL; list = list->next)
    {
	designer_node_t *node = list->data;
	GSList *slot_list;
	int i;

	lisp_print_open_paren(out);
	lisp_print_symbol("node", out);

	lisp_print_symbol(":name", out);
	lisp_print_string(node->name, out);

	lisp_print_symbol(":type", out);
	lisp_print_string(node->type->name, out);

	lisp_print_symbol(":input-slots", out);
	lisp_print_open_paren(out);
	for (i = 0, slot_list = node->type->input_slot_specs;
	     slot_list != NULL;
	     ++i, slot_list = slot_list->next)
	{
	    if (node->input_slots[i].partner != NULL)
	    {
		designer_slot_spec_t *spec = slot_list->data;

		lisp_print_open_paren(out);
		lisp_print_string(spec->name, out);
		lisp_print_string(node->input_slots[i].partner->name, out);
		lisp_print_string(node->input_slots[i].partner_slot_spec->name, out);
		lisp_print_close_paren(out);
	    }
	}
	lisp_print_close_paren(out);

	lisp_print_close_paren(out);
    }

    lisp_print_close_paren(out);
    fclose(out);

    return TRUE;
}
