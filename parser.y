/* -*- fundamental -*- */

%{
#include <stdio.h>

#include "mathmap.h"
#include "exprtree.h"
#include "builtins.h"
#include "jump.h"

extern exprtree *theExprtree;
%}

%union {
    ident ident;
    exprtree *exprtree;
}

%token T_IDENT T_STRING T_INT T_FLOAT T_RANGE
%token T_IF T_THEN T_ELSE T_END
%token T_WHILE T_DO

%left ';'
%right '='
%left T_OR T_AND
%left T_EQUAL '<' '>' T_LESSEQUAL T_GREATEREQUAL T_NOTEQUAL
%left '+' '-'
%left '*' '/' '%'
%right '^'
%left UNARY
%right ':' T_CONVERT
%left '['

%%

start :   expr              { theExprtree = $<exprtree>1; }
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
       | '-' expr %prec UNARY
                             { $<exprtree>$ = make_function("__neg", $<exprtree>2); }
       | '!' expr %prec UNARY
                             { $<exprtree>$ = make_function("__not", $<exprtree>2); }
       | '(' expr ')'        { $<exprtree>$ = $<exprtree>2; };
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
