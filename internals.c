#include <stdlib.h>
#include <string.h>

#include "tags.h"
#include "internals.h"

static internal_t *first = 0;

internal_t*
register_internal (const char *name, int number, int length)
{
    internal_t *internal = (internal_t*)malloc(sizeof(internal_t));

    strncpy(internal->name, name, MAX_INTERNAL_LENGTH);
    internal->name[MAX_INTERNAL_LENGTH] = '\0';
    internal->value.number = number;
    internal->value.length = length;
    internal->next = first;
    first = internal;

    return internal;
}

internal_t*
lookup_internal (const char *name, tuple_info_t *type)
{
    internal_t *internal;

    for (internal = first; internal != 0; internal = internal->next)
	if (strcmp(internal->name, name) == 0)
	{
	    *type = make_tuple_info(internal->value.number, internal->value.length);
	    internal->is_used = 1;
	    return internal;
	}

    return 0;
}

void
internals_clear_used (void)
{
    internal_t *internal;

    for (internal = first; internal != 0; internal = internal->next)
	internal->is_used = 0;
}
