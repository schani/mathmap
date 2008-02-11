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

#include "designer.h"

designer_design_t*
designer_load_design (designer_design_type_t *design_type, const char *filename,
		      designer_design_loaded_callback_t loaded_callback,
		      designer_node_aux_load_callback_t node_aux_load,
		      designer_design_aux_load_callback_t design_aux_load,
		      gpointer user_data)
{
    lisp_stream_t stream;
    designer_design_t *design;
    lisp_object_t *obj, *node_list, *iter, *design_aux;
    lisp_object_t *design_proplist = lisp_nil();

    if (lisp_stream_init_path(&stream, filename) == NULL)
	return NULL;
    obj = lisp_read(&stream);
    lisp_stream_free_path(&stream);

    if (!lisp_match_string("(design . #?(list))", obj, &node_list))
    {
	lisp_free(obj);
	return NULL;
    }

    design = designer_make_design(design_type);
    g_assert (design != NULL);

    /* Add all the nodes first */
    iter = node_list;
    while (!lisp_nil_p(iter))
    {
	lisp_object_t *node_proplist;

	g_assert(lisp_cons_p(iter));

	if (lisp_match_string("(node . #?(list))", lisp_car(iter), &node_proplist))
	{
	    lisp_object_t *name = lisp_proplist_lookup_symbol(node_proplist, ":name");
	    lisp_object_t *type = lisp_proplist_lookup_symbol(node_proplist, ":type");
	    designer_node_t *node;

	    g_assert(lisp_string_p(name));
	    g_assert(lisp_string_p(type));

	    node = designer_add_node(design, lisp_string(name), lisp_string(type));
	    g_assert(node != NULL);

#ifdef DEBUG_OUTPUT
	    printf("added node %s of type %s\n", lisp_string(name), lisp_string(type));
#endif
	}
	else
	{
	    design_proplist = iter;
	    break;
	}

	iter = lisp_cdr(iter);
    }

    /* Now connect the slots */
    iter = node_list;
    while (!lisp_nil_p(iter))
    {
	lisp_object_t *node_proplist;

	g_assert(lisp_cons_p(iter));

	if (lisp_match_string("(node . #?(list))", lisp_car(iter), &node_proplist))
	{
	    lisp_object_t *name = lisp_proplist_lookup_symbol(node_proplist, ":name");
	    lisp_object_t *input_slots = lisp_proplist_lookup_symbol(node_proplist, ":input-slots");
	    designer_node_t *node;

	    g_assert(lisp_string_p(name));
	    g_assert(lisp_nil_p(input_slots) || lisp_cons_p(input_slots));

	    node = designer_get_node_by_name(design, lisp_string(name));
	    g_assert(node != NULL);

	    while (!lisp_nil_p(input_slots))
	    {
		lisp_object_t *vars[3];

		g_assert(lisp_cons_p(input_slots));

		if (lisp_match_string("(#?(string) #?(string) #?(string))", lisp_car(input_slots), vars))
		{
		    designer_node_t *source_node;
		    gboolean result;

		    g_assert(lisp_string_p(vars[0]));
		    g_assert(lisp_string_p(vars[1]));
		    g_assert(lisp_string_p(vars[2]));

		    source_node = designer_get_node_by_name(design, lisp_string(vars[1]));
		    g_assert(source_node != NULL);

		    result = designer_connect_nodes(source_node, lisp_string(vars[2]),
						    node, lisp_string(vars[0]));
		    g_assert(result);
		}

		input_slots = lisp_cdr(input_slots);
	    }
	}
	else
	    break;

	iter = lisp_cdr(iter);
    }

    /* The design is loaded now */
    if (loaded_callback != NULL)
	loaded_callback(design, user_data);

    /* Go through all the nodes and call the aux callbacks */
    iter = node_list;
    while (!lisp_nil_p(iter))
    {
	lisp_object_t *node_proplist;

	g_assert(lisp_cons_p(iter));

	if (lisp_match_string("(node . #?(list))", lisp_car(iter), &node_proplist))
	{
	    lisp_object_t *name = lisp_proplist_lookup_symbol(node_proplist, ":name");
	    lisp_object_t *aux = lisp_proplist_lookup_symbol(node_proplist, ":aux");
	    designer_node_t *node;

	    g_assert(lisp_string_p(name));

	    node = designer_get_node_by_name(design, lisp_string(name));
	    g_assert(node != NULL);

	    if (!lisp_nil_p(aux) && node_aux_load != NULL)
		node_aux_load(node, aux, user_data);
	}
	else
	    break;

	iter = lisp_cdr(iter);
    }

    design_aux = lisp_proplist_lookup_symbol(design_proplist, ":aux");
    if (design_aux != NULL && design_aux_load != NULL)
	design_aux_load(design, design_aux, user_data);

    lisp_free(obj);

    return design;
}

gboolean
designer_save_design (designer_design_t *design, const char *filename,
		      designer_node_aux_print_func_t node_aux_print,
		      designer_design_aux_print_func_t design_aux_print,
		      gpointer user_data)
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

	if (node_aux_print != NULL)
	{
	    lisp_print_symbol(":aux", out);
	    node_aux_print(node, user_data, out);
	}

	lisp_print_close_paren(out);

	fputc('\n', out);
    }

    if (design_aux_print != NULL)
    {
	lisp_print_symbol(":aux", out);
	design_aux_print(design, user_data, out);
    }

    lisp_print_close_paren(out);
    fclose(out);

    return TRUE;
}
