/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_PARSER_TAB_H_INCLUDED
# define YY_YY_PARSER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    T_IDENT = 258,
    T_STRING = 259,
    T_INT = 260,
    T_FLOAT = 261,
    T_RANGE = 262,
    T_FILTER = 263,
    T_FLOAT_TYPE = 264,
    T_INT_TYPE = 265,
    T_BOOL_TYPE = 266,
    T_COLOR_TYPE = 267,
    T_GRADIENT_TYPE = 268,
    T_CURVE_TYPE = 269,
    T_IMAGE_TYPE = 270,
    T_IF = 271,
    T_THEN = 272,
    T_ELSE = 273,
    T_END = 274,
    T_WHILE = 275,
    T_DO = 276,
    T_FOR = 277,
    T_OR = 278,
    T_AND = 279,
    T_XOR = 280,
    T_EQUAL = 281,
    T_LESSEQUAL = 282,
    T_GREATEREQUAL = 283,
    T_NOTEQUAL = 284,
    UNARY = 285,
    T_CONVERT = 286
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 35 "parser.y"

    scanner_ident_t *ident;
    int arg_type;
    exprtree *exprtree;
    limits_t *limits;
    arg_decl_t *arg_decl;
    top_level_decl_t *top_level;
    option_t *options;

#line 99 "parser.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_TAB_H_INCLUDED  */
