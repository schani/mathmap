/* -*- fundamental -*- */

/*
 * parser.y
 *
 * MathMap
 *
 * Copyright (C) 1997-2004 Mark Probst
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

%{
#include <stdio.h>

#include "mathmap.h"
#include "exprtree.h"
#include "builtins.h"
#include "jump.h"
%}

%union {
    ident ident;
    exprtree *exprtree;
    arg_decl_t *arg_decl;
    top_level_decl_t *top_level;
}

%token T_IDENT T_STRING T_INT T_FLOAT T_RANGE
%token T_FILTER
%token T_IF T_THEN T_ELSE T_END
%token T_WHILE T_DO

%left ';'
%right '='
%left T_OR T_AND T_XOR
%left T_EQUAL '<' '>' T_LESSEQUAL T_GREATEREQUAL T_NOTEQUAL
%left '+' '-'
%left '*' '/' '%'
%right '^'
%left UNARY
%right ':' T_CONVERT
%left '['

%%

teststart : expr             { the_mathmap->exprtree = $<exprtree>1; }
          ;

start : filters              { /* the_mathmap->top_level_decls = $<top_level>1; */ }
      ;

filters :                    { $<top_level>$ = 0; }
        | filters filter     { $<top_level>$ = top_level_list_append($<top_level>1, $<top_level>2); }
        ;

filter : T_FILTER T_IDENT '(' args_decl ')' expr T_END
			     { $<top_level>$ = make_filter($<ident>2, $<arg_decl>4, $<exprtree>6); }
       ;

args_decl :                  { $<arg_decl>$ = 0; }
          | arg_decl_list    { $<arg_decl>$ = $<arg_decl>1; }
          ;

arg_decl_list : arg_decl     { $<arg_decl>$ = $<arg_decl>1; }
              | arg_decl_list ',' arg_decl
                             { $<arg_decl>$ = arg_decl_list_append($<arg_decl>1, $<arg_decl>3); }
              ;

arg_decl : T_IDENT T_IDENT   { $<arg_decl>$ = make_simple_arg_decl($<ident>1, $<ident>2); }
         | T_FILTER T_IDENT '(' args_decl ')'
                             { $<arg_decl>$ = make_filter_arg_decl($<ident>2, $<arg_decl>4); }
         ;

expr :   T_INT               { $<exprtree>$ = $<exprtree>1; }
       | T_FLOAT             { $<exprtree>$ = $<exprtree>1; }
       | T_IDENT             { $<exprtree>$ = make_var($<ident>1); }
       | '[' exprlist ']'    { $<exprtree>$ = make_tuple($<exprtree>2); }
       | T_IDENT '[' subscripts ']'
                             { $<exprtree>$ = make_select(make_var($<ident>1), make_tuple($<exprtree>3)); }
       | '(' expr ')' '[' subscripts ']' { $<exprtree>$ = make_select($<exprtree>2, make_tuple($<exprtree>5)); }
       | T_IDENT ':' expr    { $<exprtree>$ = make_cast($<ident>1, $<exprtree>3); }
       | T_IDENT T_CONVERT expr { $<exprtree>$ = make_convert($<ident>1, $<exprtree>3); }
       | expr '+' expr       { $<exprtree>$ = make_function("__add",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr '-' expr       { $<exprtree>$ = make_function("__sub",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr '*' expr       { $<exprtree>$ = make_function("__mul",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr '/' expr       { $<exprtree>$ = make_function("__div",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr '%' expr       { $<exprtree>$ = make_function("__mod",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr '^' expr       { $<exprtree>$ = make_function("__pow",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr T_EQUAL expr   { $<exprtree>$ = make_function("__equal",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr '<' expr       { $<exprtree>$ = make_function("__less",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr '>' expr       { $<exprtree>$ = make_function("__greater",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr T_LESSEQUAL expr
                             { $<exprtree>$ = make_function("__lessequal",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr T_GREATEREQUAL expr
                             { $<exprtree>$ = make_function("__greaterequal",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr T_NOTEQUAL expr
                             { $<exprtree>$ = make_function("__notequal",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr T_OR expr      { $<exprtree>$ = make_function("__or",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr T_AND expr     { $<exprtree>$ = make_function("__and",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | expr T_XOR expr     { $<exprtree>$ = make_function("__xor",
							    exprlist_append($<exprtree>1, $<exprtree>3)); }
       | '-' expr %prec UNARY
                             { $<exprtree>$ = make_function("__neg", $<exprtree>2); }
       | '!' expr %prec UNARY
                             { $<exprtree>$ = make_function("__not", $<exprtree>2); }
       | '(' expr ')'        { $<exprtree>$ = $<exprtree>2; }
       | T_IDENT '(' arglist ')'
                             { $<exprtree>$ = make_function($<ident>1, $<exprtree>3); }
       | T_IDENT '(' T_STRING ')'
                             { $<exprtree>$ = make_userval($<ident>1, $<ident>3, 0); }
       | T_IDENT '(' T_STRING ',' exprlist ')'
                             { $<exprtree>$ = make_userval($<ident>1, $<ident>3, $<exprtree>5); }
       | T_IDENT '=' expr    { $<exprtree>$ = make_assignment($<ident>1, $<exprtree>3); }
       | T_IDENT '[' subscripts ']' '=' expr
                             { $<exprtree>$ = make_sub_assignment($<ident>1, make_tuple($<exprtree>3), $<exprtree>6); }
       | expr ';' expr       { $<exprtree>$ = make_sequence($<exprtree>1, $<exprtree>3); }
       | T_IF expr T_THEN expr end
                             { $<exprtree>$ = make_if_then($<exprtree>2, $<exprtree>4); }
       | T_IF expr T_THEN expr else expr end
                             { $<exprtree>$ = make_if_then_else($<exprtree>2,
								$<exprtree>4,
								$<exprtree>6); }
       | T_WHILE expr T_DO expr end
                             { $<exprtree>$ = make_while($<exprtree>2, $<exprtree>4); }
       | T_DO expr T_WHILE expr end
                             { $<exprtree>$ = make_do_while($<exprtree>2, $<exprtree>4); }
       ;

arglist :                    { $<exprtree>$ = 0; }
          | exprlist         { $<exprtree>$ = $<exprtree>1; }
          ;

exprlist : expr                    { $<exprtree>$ = $<exprtree>1; }
         | exprlist ',' expr       { $<exprtree>$ = exprlist_append($<exprtree>1, $<exprtree>3); }
         ;

subscripts : subscript                 { $<exprtree>$ = $<exprtree>1; }
           | subscripts ',' subscript  { $<exprtree>$ = exprlist_append($<exprtree>1, $<exprtree>3); }
           ;

subscript : expr                   { $<exprtree>$ = $<exprtree>1; }
          | T_INT T_RANGE T_INT    { $<exprtree>$ = make_range($<exprtree>1->val.tuple_const.data[1],
                                                               $<exprtree>3->val.tuple_const.data[3]); }
          ;

else : T_ELSE
     | ';' T_ELSE
     ;

end : T_END
    | ';' T_END
    ;

%%

int
yyerror (char *s)
{
    sprintf(error_string, "Parse error.");
    JUMP(1);

    return 0;
}
