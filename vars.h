#ifndef __VARS_H__
#define __VARS_H__

#define VAR_MAX_LENGTH     32

#include "tuples.h"

typedef struct _variable_t
{
    char name[VAR_MAX_LENGTH];
    tuple_info_t type;

    tuple_t value;

    struct _variable_t *next;
} variable_t;

variable_t* register_variable (const char *name, tuple_info_t type);
variable_t* lookup_variable (const char *name, tuple_info_t *type);
variable_t* new_temporary_variable (tuple_info_t type);

void clear_all_variables (void);

extern variable_t *firstVariable;

#endif
