/* -*- c -*- */

#ifndef __BUILTINS_H__
#define __BUILTINS_H__

#include <stdio.h>

#include "tuples.h"
#include "postfix.h"

#define MAX_BUILTIN_LENGTH     63

typedef void (*builtin_function_t) (postfix_arg*);
typedef void (*generator_function_t) (FILE*, int*, int*, int);

typedef struct _builtin
{
    char name[MAX_BUILTIN_LENGTH + 1];
    builtin_function_t function;
    generator_function_t generator;
    int numParams;
    tuple_info_t tuple_info;
    struct _builtin *next;
} builtin;

double color_to_double (unsigned int red, unsigned int green,
			unsigned int blue, unsigned int alpha);
void double_to_color (double val, unsigned int *red, unsigned int *green,
		      unsigned int *blue, unsigned int *alpha);

builtin_function_t builtin_with_name (const char *name);

void init_builtins (void);

#endif
