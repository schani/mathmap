/* -*- c -*- */

/*
 * parser.y
 *
 * MathMap
 *
 * Copyright (C) 1997-2008 Mark Probst
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
    char *ident;
    int arg_type;
    exprtree *exprtree;
    limits_t *limits;
    arg_decl_t *arg_decl;
    top_level_decl_t *top_level;
    option_t *options;
}

%token T_IDENT T_STRING T_INT T_FLOAT T_RANGE
%token T_FILTER
%token T_FLOAT_TYPE T_INT_TYPE T_BOOL_TYPE T_COLOR_TYPE T_GRADIENT_TYPE T_CURVE_TYPE T_IMAGE_TYPE
%token T_IF T_THEN T_ELSE T_END
%token T_WHILE T_DO
%token T_FOR

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

start : filters
      ;

filters :
        | filters filter
        ;

filter : options_opt T_FILTER T_IDENT '(' args_decl ')' docstring_opt
                             {
				 top_level_decl_t *decl = make_filter_decl($<ident>3, $<ident>7, $<arg_decl>5,
									   $<options>1);
				 start_parsing_filter(the_mathmap, decl);
				 register_args_as_uservals(the_mathmap->current_filter, $<arg_decl>5);
			     }
         expr T_END
			     {
				 free($<ident>3);
				 if ($<ident>7 != NULL)
				     free($<ident>7);
				 the_mathmap->current_filter->v.mathmap.decl->v.filter.body = $<exprtree>9;
				 finish_parsing_filter(the_mathmap);
			     }
       ;

args_decl :                  { $<arg_decl>$ = 0; }
          | arg_decl_list    { $<arg_decl>$ = $<arg_decl>1; }
          ;

arg_decl_list : arg_decl     { $<arg_decl>$ = $<arg_decl>1; }
              | arg_decl_list ',' arg_decl
                             { $<arg_decl>$ = arg_decl_list_append($<arg_decl>1, $<arg_decl>3); }
              ;

arg_decl : options_opt type T_IDENT limits_opt default_opt docstring_opt
			     {
				 arg_decl_t *arg_decl = make_simple_arg_decl($<arg_type>2, $<ident>3, $<ident>6, $<options>1);

				 if ($<limits>4 != 0)
				 {
				     apply_limits_to_arg_decl(arg_decl, $<limits>4);
				     free_limits($<limits>4);
				 }
				 if ($<exprtree>5 != 0)
				 {
				     apply_default_to_arg_decl(arg_decl, $<exprtree>5);
				     free_exprtree($<exprtree>5);
				 }
				 free($<ident>3); if ($<ident>6 != 0) free($<ident>6);
				 $<arg_decl>$ = arg_decl;
			     }
         | options_opt T_FILTER T_IDENT '(' args_decl ')' docstring_opt
                             {
				 arg_decl_t *arg_decl = make_filter_arg_decl($<ident>3, $<arg_decl>5, $<ident>7, $<options>1);
				 free($<ident>3); if ($<ident>7 != 0) free($<ident>7);
				 $<arg_decl>$ = arg_decl;
			     }
         ;

docstring_opt :              { $<ident>$ = 0; }
	      | T_STRING     { $<ident>$ = $<ident>1; }
	      ;

options :   option
			     { $<options>$ = $<options>1; }
	  | options option
			     { $<options>$ = options_append($<options>2, $<options>1); }
	  ;

options_opt :		     { $<options>$ = 0; }
	      | options
			     { $<options>$ = $<options>1; }
	      ;

option :   T_IDENT	     { $<options>$ = make_option($<ident>1, 0); free($<ident>1); }
         | T_IDENT '(' options ')'
			     { $<options>$ = make_option($<ident>1, $<options>3); free($<ident>1); }
	 ;

int_const :   T_INT          { $<exprtree>$ = $<exprtree>1; }
            | '-' T_INT      { $<exprtree>$ = $<exprtree>2;
			       $<exprtree>$->val.int_const = -$<exprtree>$->val.int_const; }
	    ;

float_const :   T_FLOAT      { $<exprtree>$ = $<exprtree>1; }
              | '-' T_FLOAT  { $<exprtree>$ = $<exprtree>2;
			       $<exprtree>$->val.float_const = -$<exprtree>$->val.float_const; }
	      ;

limits_opt :                 { $<limits>$ = 0; }
	   | ':' int_const '-' int_const
			     { $<limits>$ = make_int_limits($<exprtree>2->val.int_const,
							    $<exprtree>4->val.int_const); }
	   | ':' float_const '-' int_const
			     { $<limits>$ = make_float_limits($<exprtree>2->val.float_const,
							      (float)$<exprtree>4->val.int_const); }
	   | ':' int_const '-' float_const
			     { $<limits>$ = make_float_limits((float)$<exprtree>2->val.int_const,
							      $<exprtree>4->val.float_const); }
	   | ':' float_const '-' float_const
			     { $<limits>$ = make_float_limits($<exprtree>2->val.float_const,
							      $<exprtree>4->val.float_const); }
	   ;

default_opt :		     { $<exprtree>$ = 0; }
	    | '(' int_const ')'
			     { $<exprtree>$ = $<exprtree>2; }
	    | '(' float_const ')'
			     { $<exprtree>$ = $<exprtree>2; }
	    ;

type :   T_FLOAT_TYPE	     { $<arg_type>$ = ARG_TYPE_FLOAT; }
       | T_INT_TYPE	     { $<arg_type>$ = ARG_TYPE_INT; }
       | T_BOOL_TYPE	     { $<arg_type>$ = ARG_TYPE_BOOL; }
       | T_COLOR_TYPE	     { $<arg_type>$ = ARG_TYPE_COLOR; }
       | T_GRADIENT_TYPE     { $<arg_type>$ = ARG_TYPE_GRADIENT; }
       | T_CURVE_TYPE	     { $<arg_type>$ = ARG_TYPE_CURVE; }
       | T_IMAGE_TYPE	     { $<arg_type>$ = ARG_TYPE_IMAGE; }
       ;

expr :   T_INT               { $<exprtree>$ = $<exprtree>1; }
       | T_FLOAT             { $<exprtree>$ = $<exprtree>1; }
       | T_IDENT             { $<exprtree>$ = make_var($<ident>1);
			       free($<ident>1); }
       | '[' exprlist ']'    { $<exprtree>$ = make_tuple_exprtree($<exprtree>2); }
       | T_IDENT '[' subscripts ']'
                             { $<exprtree>$ = make_select(make_var($<ident>1), make_tuple_exprtree($<exprtree>3));
			       free($<ident>1); }
       | '(' expr ')' '[' subscripts ']' { $<exprtree>$ = make_select($<exprtree>2, make_tuple_exprtree($<exprtree>5)); }
       | T_IDENT ':' expr    { $<exprtree>$ = make_cast($<ident>1, $<exprtree>3);
			       free($<ident>1); }
       | T_IDENT T_CONVERT expr { $<exprtree>$ = make_convert($<ident>1, $<exprtree>3);
				  free($<ident>1); }
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
                             { $<exprtree>$ = make_function($<ident>1, $<exprtree>3);
			       free($<ident>1); }
       | T_IDENT '=' expr    { $<exprtree>$ = make_assignment($<ident>1, $<exprtree>3);
			       free($<ident>1); }
       | T_IDENT '[' subscripts ']' '=' expr
                             { $<exprtree>$ = make_sub_assignment($<ident>1, make_tuple_exprtree($<exprtree>3), $<exprtree>6);
			       free($<ident>1); }
       | expr ';' expr       { $<exprtree>$ = make_sequence($<exprtree>1, $<exprtree>3); }
       | T_IF expr T_THEN expr end
                             { $<exprtree>$ = make_if_then($<exprtree>2, $<exprtree>4); }
       | T_IF expr T_THEN expr else expr end
                             { $<exprtree>$ = make_if_then_else($<exprtree>2,
								$<exprtree>4,
								$<exprtree>6); }
       | T_WHILE expr T_DO expr end
                             { $<exprtree>$ = make_while($<exprtree>2, $<exprtree>4); }
       | T_DO expr T_WHILE expr T_END
                             { $<exprtree>$ = make_do_while($<exprtree>2, $<exprtree>4); }
       | T_FOR T_IDENT '=' expr
			     {
				 check_for_start($<exprtree>4);
				 $<exprtree>$ = make_assignment($<ident>2, $<exprtree>4);
			     }
	 T_RANGE expr T_DO expr T_END
			     { $<exprtree>$ = make_for($<ident>2, $<exprtree>5, $<exprtree>4, $<exprtree>7, $<exprtree>9); }
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
          | T_INT T_RANGE T_INT    { $<exprtree>$ = make_range($<exprtree>1->val.int_const,
                                                               $<exprtree>3->val.int_const); }
          ;

else : T_ELSE
     | ';' T_ELSE
     ;

end : T_END
    | ';' T_END
    ;

%%

extern gboolean report_parse_error_to_user;

int
yyerror (char *s)
{
    sprintf(error_string, _("Parse error."));
    if (report_parse_error_to_user)
	set_expression_marker(scanner_line_num, 0);
    JUMP(1);

    return 0;
}
