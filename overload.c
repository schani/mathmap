/*
 * overload.c
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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "lispreader.h"
#include "tags.h"

#include "overload.h"

extern int intersamplingEnabled;

extern int oaparse (void);

static overload_entry_t *first_overload_entry = 0,
    *last_overload_entry = 0;
static named_binding_t *first_named_binding = 0;

binding_t*
new_free_variable_binding (void)
{
    binding_t *binding = (binding_t*)malloc(sizeof(binding_t));

    binding->is_bound = 0;
    binding->next = 0;

    return binding;
}

binding_t*
new_constant_binding (int value)
{
    binding_t *binding = (binding_t*)malloc(sizeof(binding_t));

    binding->is_bound = 1;
    binding->value = value;
    binding->next = 0;

    return binding;
}

void
clear_bindings (void)
{
    named_binding_t *binding = first_named_binding;

    while (binding != 0)
    {
	named_binding_t *next = binding->next;

	free(binding);
	binding = next;
    }

    first_named_binding = 0;
}

binding_t*
free_binding_with_name (const char *name)
{
    named_binding_t *binding;

    for (binding = first_named_binding; binding != 0; binding = binding->next)
	if (strcmp(binding->name, name) == 0)
	    return binding->binding;

    binding = (named_binding_t*)malloc(sizeof(named_binding_t));
    strncpy(binding->name, name, MAX_BINDING_LENGTH);
    binding->name[MAX_BINDING_LENGTH] = '\0';
    binding->binding = new_free_variable_binding();

    binding->next = first_named_binding;
    first_named_binding = binding;

    return binding->binding;
}

overload_arg_t*
new_overload_argument (binding_t *tag, binding_t *length, overload_arg_t *next)
{
    overload_arg_t *arg = (overload_arg_t*)malloc(sizeof(overload_arg_t));

    arg->tag = tag;
    arg->length = length;
    arg->next = next;

    return arg;
}

binding_t*
string_to_binding (const char *str)
{
    if (strcmp(str, "_") == 0)
	return new_free_variable_binding();
    if (islower(str[0]))
	return new_constant_binding(tag_number_for_name(str));
    return free_binding_with_name(str);
}

binding_t*
lisp_object_to_binding (lisp_object_t *obj)
{
    switch (lisp_type(obj))
    {
	case LISP_TYPE_SYMBOL :
	    return string_to_binding(lisp_symbol(obj));
	case LISP_TYPE_INTEGER :
	    return new_constant_binding(lisp_integer(obj));
	default :
	    assert(0);
    }
    return 0;
}

overload_arg_t*
lisp_object_to_overload_arg (lisp_object_t *obj, overload_arg_t *next)
{
    return new_overload_argument(lisp_object_to_binding(lisp_car(obj)),
				 lisp_object_to_binding(lisp_car(lisp_cdr(obj))),
				 next);
}

overload_arg_t*
lisp_object_to_overload_args (lisp_object_t *obj)
{
    if (lisp_type(obj) == LISP_TYPE_NIL)
	return 0;
    return lisp_object_to_overload_arg(lisp_car(obj), lisp_object_to_overload_args(lisp_cdr(obj)));
}

void
interpret_arg_string (const char *string, overload_arg_t **result, overload_arg_t **args)
{
    lisp_object_t *obj = lisp_read_from_string(string);

    *result = lisp_object_to_overload_arg(lisp_car(obj), 0);
    *args = lisp_object_to_overload_args(lisp_cdr(obj));

    lisp_free(obj);
}

void
register_overloaded_builtin (const char *name, const char *argstring,
			     builtin_function_t func, generator_function_t gen)
{
    overload_entry_t *entry = (overload_entry_t*)malloc(sizeof(overload_entry_t));
    overload_arg_t *arg;

    strncpy(entry->name, name, MAX_FUNCTION_LENGTH);
    entry->name[MAX_FUNCTION_LENGTH] = '\0';
    entry->type = OVERLOAD_BUILTIN;

    interpret_arg_string(argstring, &entry->result, &entry->args);

    for (arg = entry->args, entry->num_args = 0; arg != 0; arg = arg->next)
	++entry->num_args;
    entry->v.builtin.builtin = func;
    entry->v.builtin.generator = gen;

    entry->next = 0;

    if (first_overload_entry == 0)
	first_overload_entry = last_overload_entry = entry;
    else
	last_overload_entry = last_overload_entry->next = entry;
}

void
register_overloaded_macro (const char *name, const char *argstring, macro_function_t func)
{
    overload_entry_t *entry = (overload_entry_t*)malloc(sizeof(overload_entry_t));
    overload_arg_t *arg;

    strncpy(entry->name, name, MAX_FUNCTION_LENGTH);
    entry->name[MAX_FUNCTION_LENGTH] = '\0';
    entry->type = OVERLOAD_MACRO;

    interpret_arg_string(argstring, &entry->result, &entry->args);

    for (arg = entry->args, entry->num_args = 0; arg != 0; arg = arg->next)
	++entry->num_args;
    entry->v.macro = func;

    entry->next = 0;

    if (first_overload_entry == 0)
	first_overload_entry = last_overload_entry = entry;
    else
	last_overload_entry = last_overload_entry->next = entry;
}

overload_entry_t*
overloaded_builtin_with_function (builtin_function_t function)
{
    overload_entry_t *entry;

    for (entry = first_overload_entry; entry != 0; entry = entry->next)
	if (entry->type == OVERLOAD_BUILTIN && entry->v.builtin.builtin == function)
	    return entry;

    return 0;
}

overload_entry_t*
resolve_function_call (const char *name, function_arg_info_t *args, tuple_info_t *result)
{
    binding_t **undo_array;
    int num_undos = 0,
	num_args = 0;
    function_arg_info_t *func_arg;
    overload_entry_t *entry;
    overload_arg_t *ovld_arg;

    for (func_arg = args; func_arg != 0; func_arg = func_arg->next)
	++num_args;

    undo_array = (binding_t**)malloc(2 * num_args * sizeof(binding_t*));

    for (entry = first_overload_entry; entry != 0; entry = entry->next)
	if (strcmp(entry->name, name) == 0 && entry->num_args == num_args)
	{
	    int match = 1;

	    for (ovld_arg = entry->args, func_arg = args;
		 func_arg != 0;
		 ovld_arg = ovld_arg->next, func_arg = func_arg->next)
	    {
		if (ovld_arg->tag->is_bound && ovld_arg->tag->value != func_arg->info.number)
		{
		    match = 0;
		    break;
		}
		else if (!ovld_arg->tag->is_bound)
		{
		    undo_array[num_undos++] = ovld_arg->tag;
		    ovld_arg->tag->is_bound = 1;
		    ovld_arg->tag->value = func_arg->info.number;
		}

		if (ovld_arg->length->is_bound && ovld_arg->length->value != func_arg->info.length)
		{
		    match = 0;
		    break;
		}
		else if (!ovld_arg->length->is_bound)
		{
		    undo_array[num_undos++] = ovld_arg->length;
		    ovld_arg->length->is_bound = 1;
		    ovld_arg->length->value = func_arg->info.length;
		}
	    }

	    if (match)
	    {
		assert(entry->result->tag->is_bound && entry->result->length->is_bound);
		*result = make_tuple_info(entry->result->tag->value, entry->result->length->value);
	    }

	    for (--num_undos; num_undos >= 0; --num_undos)
		undo_array[num_undos]->is_bound = 0;
	    num_undos = 0;

	    if (match)
	    {
		free(undo_array);
		return entry;
	    }
	}

    free(undo_array);
    return 0;
}
