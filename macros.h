/* -*- c -*- */

#ifndef __MACROS_H__
#define __MACROS_H__

#include "tuples.h"

#define MAX_MACRO_LENGTH     63

struct _exprtree;

typedef struct _exprtree* (*macro_function_t) (struct _exprtree *args);

typedef struct _macro_t
{
    char name[MAX_MACRO_LENGTH + 1];
    tuple_info_t info;

    macro_function_t function;

    struct _macro_t *next;
} var_macro_t;

macro_function_t lookup_variable_macro (const char *name, tuple_info_t *info);

void init_macros (void);

#endif
