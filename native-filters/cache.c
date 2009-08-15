/* -*- c -*- */

/*
 * cache.c
 *
 * MathMap
 *
 * Copyright (C) 2009 Mark Probst
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

#include "../mathmap.h"

static filter_t*
get_native_filter_for_func (mathmap_t *mathmap, native_filter_func_t func)
{
    filter_t *filter;

    for (filter = mathmap->filters; filter != NULL; filter = filter->next)
	if (filter->kind == FILTER_NATIVE && filter->v.native.func == func)
	    return filter;
    g_assert_not_reached();
}

static gboolean
uservals_match (filter_t *filter, userval_t *entry_args, userval_t *filter_args)
{
    int i;
    userval_info_t *info;

    for (i = 0, info = filter->userval_infos;
	 i < filter->num_uservals;
	 ++i, info = info->next)
    {
	switch (info->type)
	{
	    case USERVAL_INT_CONST :
		if (entry_args[i].v.int_const != filter_args[i].v.int_const)
		    return FALSE;
		break;

	    case USERVAL_FLOAT_CONST :
		if (entry_args[i].v.float_const != filter_args[i].v.float_const)
		    return FALSE;
		break;

	    case USERVAL_BOOL_CONST :
		if (entry_args[i].v.bool_const != filter_args[i].v.bool_const)
		    return FALSE;
		break;

	    case USERVAL_IMAGE :
		if (GPOINTER_TO_INT(entry_args[i].v.image) != filter_args[i].v.image->id)
		    return FALSE;
		break;

	    default :
		g_assert_not_reached();
	}
    }
    g_assert(info == NULL);

    return TRUE;
}

static userval_t*
uservals_copy (filter_t *filter, userval_t *args, mathmap_pools_t *pools)
{
    int i;
    userval_info_t *info;
    userval_t *copy = mathmap_pools_alloc(pools, sizeof(userval_t) * filter->num_uservals);

    for (i = 0, info = filter->userval_infos;
	 i < filter->num_uservals;
	 ++i, info = info->next)
    {
	switch (info->type)
	{
	    case USERVAL_INT_CONST :
	    case USERVAL_FLOAT_CONST :
	    case USERVAL_BOOL_CONST :
		copy_userval(&copy[i], &args[i], info->type);
		break;

	    case USERVAL_IMAGE :
		copy[i].v.image = GINT_TO_POINTER(args[i].v.image->id);
		break;

	    default :
		g_assert_not_reached();
	}
    }

    return copy;
}

native_filter_cache_entry_t*
invocation_lookup_native_filter_invocation (mathmap_invocation_t *invocation, userval_t *args,
					    native_filter_func_t filter_func)
{
    filter_t *filter = get_native_filter_for_func(invocation->mathmap, filter_func);
    native_filter_cache_entry_t *entry;

    g_mutex_lock(invocation->native_filter_cache_mutex);

    for (entry = invocation->native_filter_cache; entry != NULL; entry = entry->next)
    {
	if (entry->filter != filter)
	    continue;

	if (uservals_match(filter, entry->args, args))
	    break;
    }

    if (entry)
    {
	while (entry->image == NULL)
	    g_cond_wait(invocation->native_filter_cache_cond, invocation->native_filter_cache_mutex);
    }
    else
    {
	entry = mathmap_pools_alloc(&invocation->pools, sizeof(native_filter_cache_entry_t));
	entry->filter = filter;
	entry->args = uservals_copy(filter, args, &invocation->pools);
	entry->image = NULL;
	entry->next = invocation->native_filter_cache;
	invocation->native_filter_cache = entry;
    }

    g_mutex_unlock(invocation->native_filter_cache_mutex);

    return entry;
}

void
native_filter_cache_entry_set_image (mathmap_invocation_t *invocation, native_filter_cache_entry_t *cache_entry, image_t *image)
{
    g_mutex_lock(invocation->native_filter_cache_mutex);
    g_assert(cache_entry->image == NULL);
    cache_entry->image = image;
    g_cond_broadcast(invocation->native_filter_cache_cond);
    g_mutex_unlock(invocation->native_filter_cache_mutex);
}
