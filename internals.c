#include <stdlib.h>
#include <string.h>

#include "tags.h"
#include "internals.h"

static internal_t *first = 0,
    *xy_internal = 0,
    *ra_internal = 0;

extern int usesRA;
extern double currentX,
    currentY,
    currentT,
    currentR,
    currentA,
    imageR,
    imageX,
    imageY,
    imageW,
    imageH;

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

    if (strcmp(name, "ra") == 0)
	usesRA = 1;

    for (internal = first; internal != 0; internal = internal->next)
	if (strcmp(internal->name, name) == 0)
	{
	    *type = make_tuple_info(internal->value.number, internal->value.length);
	    return internal;
	}

    return 0;
}

void
init_internals (void)
{
    xy_internal = register_internal("xy", xy_tag_number, 2);
    ra_internal = register_internal("ra", ra_tag_number, 2);
    register_internal("t", nil_tag_number, 1);
    register_internal("XY", xy_tag_number, 2);
    register_internal("WH", xy_tag_number, 2);
    register_internal("R", nil_tag_number, 1);
}

void
update_image_internals (void)
{
    internal_t *internal;
    tuple_info_t dummy;

    internal = lookup_internal("t", &dummy);
    internal->value.data[0] = currentT;

    internal = lookup_internal("XY", &dummy);
    internal->value.data[0] = imageX;
    internal->value.data[1] = imageY;
    
    internal = lookup_internal("WH", &dummy);
    internal->value.data[0] = imageW;
    internal->value.data[1] = imageH;
    
    internal = lookup_internal("R", &dummy);
    internal->value.data[0] = imageR;
}

void
update_pixel_internals (void)
{
    xy_internal->value.data[0] = currentX;
    xy_internal->value.data[1] = currentY;

    ra_internal->value.data[0] = currentR;
    ra_internal->value.data[1] = currentA;
}
