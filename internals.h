/* -*- c -*- */

#ifndef __INTERNALS_H__
#define __INTERNALS_H__

#include "tuples.h"

#define MAX_INTERNAL_LENGTH    63

typedef struct _internal_t
{
    char name[MAX_INTERNAL_LENGTH + 1];
    tuple_t value;
    struct _internal_t *next;
} internal_t;

internal_t* register_internal (const char *name, int number, int length);
internal_t* lookup_internal (const char *name, tuple_info_t *type);

void init_internals (void);
void update_image_internals (void);
void update_pixel_internals (void);

#endif
