/* -*- c -*- */

#include <stdlib.h>
#include <string.h>

#include "tags.h"
#include "exprtree.h"

int nil_tag_number,
    xy_tag_number,
    ra_tag_number,
    rgba_tag_number,
    ri_tag_number,
    image_tag_number;

typedef struct _tag_entry
{
    ident name;
    int number;

    struct _tag_entry *next;
} tag_entry;

static tag_entry *first = 0;
static int num_tags = 0;

void
init_tags (void)
{
    nil_tag_number = tag_number_for_name("nil");
    xy_tag_number = tag_number_for_name("xy");
    ra_tag_number = tag_number_for_name("ra");
    rgba_tag_number = tag_number_for_name("rgba");
    ri_tag_number = tag_number_for_name("ri");
    image_tag_number = tag_number_for_name("image");
}

int
tag_number_for_name (const char *name)
{
    tag_entry *entry;

    for (entry = first; entry != 0; entry = entry->next)
    {
	if (strcmp(name, entry->name) == 0)
	    return entry->number;
    }

    entry = (tag_entry*)malloc(sizeof(tag_entry));

    strncpy(entry->name, name, MAX_IDENT_LENGTH);
    entry->name[MAX_IDENT_LENGTH] = '\0';
    entry->number = ++num_tags;
    entry->next = first;
    first = entry;

    return entry->number;
}

const char*
tag_name_for_number (int num)
{
    tag_entry *entry;

    for (entry = first; entry != 0; entry = entry->next)
    {
	if (entry->number == num)
	    return entry->name;
    }

    return 0;
}
