/* -*- fundamental -*- */

%{
#include <stdio.h>

#include "overload.h"

overload_arg_t *overload_result,
    *overload_args;
%}

%union {
    overload_arg_t *arg;
    binding_t *binding;
}

%token T_BINDING

%%

start :   result '=' args                     { overload_result = $<arg>1; overload_args = $<arg>3; }
        ;

tuple :   T_BINDING ':' T_BINDING             { $<arg>$ = new_overload_argument($<binding>1, $<binding>3, 0); }
        ;

result :   tuple
         ;

args :   arg
       | arg ',' args                         { $<arg>$ = $<arg>1; $<arg>$->next = $<arg>3; }
       ;

arg :   tuple
      ;

%%

int
yyerror (char *s)
{
    fprintf(stderr, "%s\n", s);

    return 0;
}
