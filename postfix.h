#ifndef __POSTFIX_H__
#define __POSTFIX_H__

#include "tuples.h"
#include "vars.h"
#include "internals.h"

typedef union
{
    int integer;
    tuple_t tuple;
    internal_t *internal;
    variable_t *user_var;
} postfix_arg;

typedef void (*stackfunc) (postfix_arg*);

typedef struct _postfix
{
    stackfunc func;
    postfix_arg arg;
} postfix;

extern tuple_t stack[];
extern int stackp;

extern postfix expression[];
extern int exprp;

extern int num_ops;

struct _exprtree;

void make_postfix (struct _exprtree *tree);
void make_empty_postfix (void);
void output_postfix (void);
tuple_t* eval_postfix (void);

void make_postfix_recursive (struct _exprtree *tree);

void stack_push (postfix_arg *arg);
void stack_select_i (postfix_arg *arg);
void stack_tuple (postfix_arg *arg);
void stack_dupn_i (postfix_arg *arg);
void stack_cast (postfix_arg *arg);
void stack_push_internal (postfix_arg *arg);

#endif
