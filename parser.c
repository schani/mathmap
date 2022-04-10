/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 25 "parser.y"

#include <stdio.h>

#include "mathmap.h"
#include "exprtree.h"
#include "builtins/builtins.h"
#include "jump.h"
#include "scanner.h"

#line 80 "parser.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
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

#line 174 "parser.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_TAB_H_INCLUDED  */



#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   441

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  49
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  25
/* YYNRULES -- Number of rules.  */
#define YYNRULES  87
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  172

#define YYUNDEFTOK  2
#define YYMAXUTOK   286


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    48,     2,     2,     2,    38,     2,     2,
      44,    45,    36,    34,    46,    35,     2,    37,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    41,    23,
      29,    24,    30,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    43,     2,    47,    39,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    25,    26,
      27,    28,    31,    32,    33,    40,    42
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    65,    65,    68,    69,    73,    72,    89,    90,    93,
      94,    98,   116,   125,   126,   129,   131,   135,   136,   140,
     141,   145,   146,   150,   151,   155,   156,   158,   160,   162,
     166,   167,   169,   173,   174,   175,   176,   177,   178,   179,
     182,   183,   184,   185,   186,   191,   192,   194,   195,   196,
     197,   198,   199,   200,   201,   202,   203,   204,   206,   208,
     210,   211,   212,   213,   215,   217,   218,   221,   223,   226,
     227,   229,   233,   235,   238,   237,   249,   250,   253,   254,
     257,   258,   261,   262,   265,   266,   269,   270
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "T_IDENT", "T_STRING", "T_INT",
  "T_FLOAT", "T_RANGE", "T_FILTER", "T_FLOAT_TYPE", "T_INT_TYPE",
  "T_BOOL_TYPE", "T_COLOR_TYPE", "T_GRADIENT_TYPE", "T_CURVE_TYPE",
  "T_IMAGE_TYPE", "T_IF", "T_THEN", "T_ELSE", "T_END", "T_WHILE", "T_DO",
  "T_FOR", "';'", "'='", "T_OR", "T_AND", "T_XOR", "T_EQUAL", "'<'", "'>'",
  "T_LESSEQUAL", "T_GREATEREQUAL", "T_NOTEQUAL", "'+'", "'-'", "'*'",
  "'/'", "'%'", "'^'", "UNARY", "':'", "T_CONVERT", "'['", "'('", "')'",
  "','", "']'", "'!'", "$accept", "start", "filters", "filter", "$@1",
  "args_decl", "arg_decl_list", "arg_decl", "docstring_opt", "options",
  "options_opt", "option", "int_const", "float_const", "limits_opt",
  "default_opt", "type", "expr", "@2", "arglist", "exprlist", "subscripts",
  "subscript", "else", "end", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,    59,    61,   278,   279,   280,   281,    60,
      62,   282,   283,   284,    43,    45,    42,    47,    37,    94,
     285,    58,   286,    91,    40,    41,    44,    93,    33
};
# endif

#define YYPACT_NINF (-142)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-18)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -142,     6,    14,  -142,   -34,  -142,    17,    10,  -142,    17,
    -142,    30,    12,    -8,  -142,    13,    20,    25,  -142,   426,
      57,    17,    75,  -142,  -142,  -142,  -142,  -142,  -142,  -142,
      81,  -142,  -142,  -142,    56,    58,   125,    13,    -1,    59,
      44,  -142,  -142,   125,   125,   125,    92,   125,   125,   125,
     125,   241,    62,  -142,  -142,    54,    66,    67,    -1,    57,
     125,   125,   125,   156,   125,   180,   343,   324,    82,  -142,
     379,    16,   197,  -142,  -142,   125,   125,   125,   125,   125,
     125,   125,   125,   125,   125,   125,   125,   125,   125,   125,
     125,    57,  -142,  -142,    -1,    -1,    68,    70,  -142,   394,
    -142,  -142,   109,   379,    23,  -142,    74,    76,   125,   125,
     125,   125,   125,  -142,    69,   394,   151,   151,   151,    55,
      55,    55,    55,    55,    55,    -7,    -7,    84,    84,    84,
      84,  -142,  -142,  -142,  -142,  -142,  -142,  -142,   127,   156,
     110,  -142,   220,   262,   283,   379,   379,   156,  -142,  -142,
     125,  -142,  -142,    61,   125,  -142,   105,  -142,  -142,   128,
      51,   394,  -142,  -142,   262,   125,  -142,  -142,   362,   125,
     304,  -142
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       3,     0,     2,     1,    19,     4,    18,     0,    15,     0,
      16,     0,     0,     0,    20,    17,     0,     8,     9,     0,
      13,    17,     0,    33,    34,    35,    36,    37,    38,    39,
       0,    14,     5,    10,     0,    25,     0,    17,     0,    30,
      42,    40,    41,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    21,    23,     0,     0,     0,     0,    13,
       0,     0,     0,     0,    76,     0,     0,     0,     0,    63,
      78,     0,     0,    64,     6,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    22,    24,     0,     0,     0,     0,    11,    67,
      46,    47,    40,    82,     0,    80,     0,    77,     0,     0,
       0,     0,     0,    43,    65,    69,    60,    61,    62,    54,
      55,    56,    57,    58,    59,    48,    49,    50,    51,    52,
      53,    12,    26,    28,    27,    29,    31,    32,     0,     0,
      44,    66,     0,     0,     0,    74,    79,     0,    83,    81,
       0,    84,    86,     0,     0,    70,     0,    72,    73,     0,
       0,    68,    85,    87,     0,     0,    45,    71,     0,     0,
       0,    75
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -142,  -142,  -142,  -142,  -142,    99,  -142,   116,   -56,   129,
     137,    15,   -57,   -39,  -142,  -142,  -142,   -36,  -142,  -142,
      78,    -4,     5,  -142,  -141
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     5,    36,    16,    17,    18,    32,     6,
      19,     8,    56,    57,    39,    59,    30,   103,   159,   106,
      71,   104,   105,   154,   155
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      51,    96,   157,    98,    53,    54,     3,    65,    66,    67,
       9,    69,    70,    72,    73,     4,     4,     4,    11,    97,
       4,    10,   -17,   167,    99,   100,   101,    10,    70,    87,
      88,    89,    90,    13,    55,   131,    15,   132,   134,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   133,   135,    14,    -7,    92,
      93,    31,   112,   113,    40,    20,    41,    42,    60,   139,
     140,    21,   142,   143,   144,   145,   146,    43,    34,   162,
     163,    44,    45,    46,    35,    61,    62,    63,    64,    85,
      86,    87,    88,    89,    90,    68,    47,   139,   166,    38,
      37,    94,    95,    58,    48,    49,   111,    91,    40,    50,
      41,    42,   147,   136,   161,   137,   138,   115,   164,   141,
     115,    43,   112,    90,   163,    44,    45,    46,    40,   168,
      41,    42,   148,   170,   150,   165,    52,    33,    12,     7,
      47,    43,   107,   160,   149,    44,    45,    46,    48,    49,
       0,     0,     0,    50,     0,     0,     0,     0,     0,    40,
      47,   102,    42,     0,     0,     0,     0,     0,    48,    49,
       0,     0,    43,    50,     0,     0,    44,    45,    46,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    47,     0,     0,     0,     0,     0,   108,     0,    48,
      49,     0,     0,    75,    50,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      75,     0,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,     0,   151,   152,
       0,     0,   114,   153,     0,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      74,     0,     0,     0,    75,     0,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,   152,     0,     0,     0,   156,     0,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,   158,     0,     0,     0,    75,     0,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,   171,     0,     0,     0,    75,     0,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,   110,     0,     0,    75,     0,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,   109,     0,    75,     0,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,   169,     0,    75,     0,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    75,     0,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    22,    23,    24,    25,    26,    27,
      28,    29
};

static const yytype_int16 yycheck[] =
{
      36,    58,   143,    59,     5,     6,     0,    43,    44,    45,
      44,    47,    48,    49,    50,     3,     3,     3,     8,    58,
       3,     6,     8,   164,    60,    61,    62,    12,    64,    36,
      37,    38,    39,     3,    35,    91,    44,    94,    95,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    94,    95,    45,    45,     5,
       6,     4,    46,    47,     3,    45,     5,     6,    24,    46,
      47,    46,   108,   109,   110,   111,   112,    16,     3,    18,
      19,    20,    21,    22,     3,    41,    42,    43,    44,    34,
      35,    36,    37,    38,    39,     3,    35,    46,    47,    41,
      44,    35,    35,    44,    43,    44,    24,    45,     3,    48,
       5,     6,    43,    45,   150,    45,     7,   153,   154,    45,
     156,    16,    46,    39,    19,    20,    21,    22,     3,   165,
       5,     6,     5,   169,    24,     7,    37,    21,     9,     2,
      35,    16,    64,   147,   139,    20,    21,    22,    43,    44,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,     3,
      35,     5,     6,    -1,    -1,    -1,    -1,    -1,    43,    44,
      -1,    -1,    16,    48,    -1,    -1,    20,    21,    22,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    35,    -1,    -1,    -1,    -1,    -1,    17,    -1,    43,
      44,    -1,    -1,    23,    48,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      23,    -1,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    18,    19,
      -1,    -1,    45,    23,    -1,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      19,    -1,    -1,    -1,    23,    -1,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    19,    -1,    -1,    -1,    23,    -1,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    19,    -1,    -1,    -1,    23,    -1,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    19,    -1,    -1,    -1,    23,    -1,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    20,    -1,    -1,    23,    -1,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    21,    -1,    23,    -1,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    21,    -1,    23,    -1,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    23,    -1,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     8,     9,    10,    11,    12,    13,
      14,    15
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    50,    51,     0,     3,    52,    58,    59,    60,    44,
      60,     8,    58,     3,    45,    44,    54,    55,    56,    59,
      45,    46,     8,     9,    10,    11,    12,    13,    14,    15,
      65,     4,    57,    56,     3,     3,    53,    44,    41,    63,
       3,     5,     6,    16,    20,    21,    22,    35,    43,    44,
      48,    66,    54,     5,     6,    35,    61,    62,    44,    64,
      24,    41,    42,    43,    44,    66,    66,    66,     3,    66,
      66,    69,    66,    66,    19,    23,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    45,     5,     6,    35,    35,    61,    62,    57,    66,
      66,    66,     5,    66,    70,    71,    68,    69,    17,    21,
      20,    24,    46,    47,    45,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    57,    61,    62,    61,    62,    45,    45,     7,    46,
      47,    45,    66,    66,    66,    66,    66,    43,     5,    71,
      24,    18,    19,    23,    72,    73,    23,    73,    19,    67,
      70,    66,    18,    19,    66,     7,    47,    73,    66,    21,
      66,    19
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    49,    50,    51,    51,    53,    52,    54,    54,    55,
      55,    56,    56,    57,    57,    58,    58,    59,    59,    60,
      60,    61,    61,    62,    62,    63,    63,    63,    63,    63,
      64,    64,    64,    65,    65,    65,    65,    65,    65,    65,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    67,    66,    68,    68,    69,    69,
      70,    70,    71,    71,    72,    72,    73,    73
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     0,     2,     0,    10,     0,     1,     1,
       3,     6,     7,     0,     1,     1,     2,     0,     1,     1,
       4,     1,     2,     1,     2,     0,     4,     4,     4,     4,
       0,     3,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     4,     6,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     2,     2,     3,     4,     3,     6,     3,
       5,     7,     5,     5,     0,    10,     0,     1,     1,     3,
       1,     3,     1,     3,     1,     2,     1,     2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 5:
#line 73 "parser.y"
                             {
				 top_level_decl_t *decl = make_filter_decl((yyvsp[-4].ident), (yyvsp[0].ident), (yyvsp[-2].arg_decl),
									   (yyvsp[-6].options));
				 start_parsing_filter(the_mathmap, decl);
				 register_args_as_uservals(the_mathmap->current_filter, (yyvsp[-2].arg_decl));
			     }
#line 1525 "parser.tab.c"
    break;

  case 6:
#line 80 "parser.y"
                             {
				 free((yyvsp[-7].ident));
				 if ((yyvsp[-3].ident) != NULL)
				     free((yyvsp[-3].ident));
				 the_mathmap->current_filter->v.mathmap.decl->v.filter.body = (yyvsp[-1].exprtree);
				 finish_parsing_filter(the_mathmap);
			     }
#line 1537 "parser.tab.c"
    break;

  case 7:
#line 89 "parser.y"
                             { (yyval.arg_decl) = 0; }
#line 1543 "parser.tab.c"
    break;

  case 8:
#line 90 "parser.y"
                             { (yyval.arg_decl) = (yyvsp[0].arg_decl); }
#line 1549 "parser.tab.c"
    break;

  case 9:
#line 93 "parser.y"
                             { (yyval.arg_decl) = (yyvsp[0].arg_decl); }
#line 1555 "parser.tab.c"
    break;

  case 10:
#line 95 "parser.y"
                             { (yyval.arg_decl) = arg_decl_list_append((yyvsp[-2].arg_decl), (yyvsp[0].arg_decl)); }
#line 1561 "parser.tab.c"
    break;

  case 11:
#line 99 "parser.y"
                             {
				 arg_decl_t *arg_decl = make_simple_arg_decl((yyvsp[-4].arg_type), (yyvsp[-3].ident),
									     (yyvsp[0].ident), (yyvsp[-5].options));

				 if ((yyvsp[-2].limits) != 0)
				 {
				     apply_limits_to_arg_decl(arg_decl, (yyvsp[-2].limits));
				     free_limits((yyvsp[-2].limits));
				 }
				 if ((yyvsp[-1].exprtree) != 0)
				 {
				     apply_default_to_arg_decl(arg_decl, (yyvsp[-1].exprtree));
				     free_exprtree((yyvsp[-1].exprtree));
				 }
				 free((yyvsp[-3].ident)); if ((yyvsp[0].ident) != NULL) free((yyvsp[0].ident));
				 (yyval.arg_decl) = arg_decl;
			     }
#line 1583 "parser.tab.c"
    break;

  case 12:
#line 117 "parser.y"
                             {
				 arg_decl_t *arg_decl = make_filter_arg_decl((yyvsp[-4].ident), (yyvsp[-2].arg_decl),
									     (yyvsp[0].ident), (yyvsp[-6].options));
				 free((yyvsp[-4].ident)); if ((yyvsp[0].ident) != NULL) free((yyvsp[0].ident));
				 (yyval.arg_decl) = arg_decl;
			     }
#line 1594 "parser.tab.c"
    break;

  case 13:
#line 125 "parser.y"
                             { (yyval.ident) = NULL; }
#line 1600 "parser.tab.c"
    break;

  case 14:
#line 126 "parser.y"
                             { (yyval.ident) = (yyvsp[0].ident); }
#line 1606 "parser.tab.c"
    break;

  case 15:
#line 130 "parser.y"
                             { (yyval.options) = (yyvsp[0].options); }
#line 1612 "parser.tab.c"
    break;

  case 16:
#line 132 "parser.y"
                             { (yyval.options) = options_append((yyvsp[0].options), (yyvsp[-1].options)); }
#line 1618 "parser.tab.c"
    break;

  case 17:
#line 135 "parser.y"
                             { (yyval.options) = 0; }
#line 1624 "parser.tab.c"
    break;

  case 18:
#line 137 "parser.y"
                             { (yyval.options) = (yyvsp[0].options); }
#line 1630 "parser.tab.c"
    break;

  case 19:
#line 140 "parser.y"
                             { (yyval.options) = make_option((yyvsp[0].ident), 0); free((yyvsp[0].ident)); }
#line 1636 "parser.tab.c"
    break;

  case 20:
#line 142 "parser.y"
                             { (yyval.options) = make_option((yyvsp[-3].ident), (yyvsp[-1].options)); free((yyvsp[-3].ident)); }
#line 1642 "parser.tab.c"
    break;

  case 21:
#line 145 "parser.y"
                             { (yyval.exprtree) = (yyvsp[0].exprtree); }
#line 1648 "parser.tab.c"
    break;

  case 22:
#line 146 "parser.y"
                             { (yyval.exprtree) = (yyvsp[0].exprtree);
			       (yyval.exprtree)->val.int_const = -(yyval.exprtree)->val.int_const; }
#line 1655 "parser.tab.c"
    break;

  case 23:
#line 150 "parser.y"
                             { (yyval.exprtree) = (yyvsp[0].exprtree); }
#line 1661 "parser.tab.c"
    break;

  case 24:
#line 151 "parser.y"
                             { (yyval.exprtree) = (yyvsp[0].exprtree);
			       (yyval.exprtree)->val.float_const = -(yyval.exprtree)->val.float_const; }
#line 1668 "parser.tab.c"
    break;

  case 25:
#line 155 "parser.y"
                             { (yyval.limits) = 0; }
#line 1674 "parser.tab.c"
    break;

  case 26:
#line 157 "parser.y"
                             { (yyval.limits) = make_int_limits((yyvsp[-2].exprtree), (yyvsp[0].exprtree)); }
#line 1680 "parser.tab.c"
    break;

  case 27:
#line 159 "parser.y"
                             { (yyval.limits) = make_float_limits((yyvsp[-2].exprtree), (yyvsp[0].exprtree)); }
#line 1686 "parser.tab.c"
    break;

  case 28:
#line 161 "parser.y"
                             { (yyval.limits) = make_float_limits((yyvsp[-2].exprtree), (yyvsp[0].exprtree)); }
#line 1692 "parser.tab.c"
    break;

  case 29:
#line 163 "parser.y"
                             { (yyval.limits) = make_float_limits((yyvsp[-2].exprtree), (yyvsp[0].exprtree)); }
#line 1698 "parser.tab.c"
    break;

  case 30:
#line 166 "parser.y"
                             { (yyval.exprtree) = 0; }
#line 1704 "parser.tab.c"
    break;

  case 31:
#line 168 "parser.y"
                             { (yyval.exprtree) = (yyvsp[-1].exprtree); }
#line 1710 "parser.tab.c"
    break;

  case 32:
#line 170 "parser.y"
                             { (yyval.exprtree) = (yyvsp[-1].exprtree); }
#line 1716 "parser.tab.c"
    break;

  case 33:
#line 173 "parser.y"
                             { (yyval.arg_type) = ARG_TYPE_FLOAT; }
#line 1722 "parser.tab.c"
    break;

  case 34:
#line 174 "parser.y"
                             { (yyval.arg_type) = ARG_TYPE_INT; }
#line 1728 "parser.tab.c"
    break;

  case 35:
#line 175 "parser.y"
                             { (yyval.arg_type) = ARG_TYPE_BOOL; }
#line 1734 "parser.tab.c"
    break;

  case 36:
#line 176 "parser.y"
                             { (yyval.arg_type) = ARG_TYPE_COLOR; }
#line 1740 "parser.tab.c"
    break;

  case 37:
#line 177 "parser.y"
                             { (yyval.arg_type) = ARG_TYPE_GRADIENT; }
#line 1746 "parser.tab.c"
    break;

  case 38:
#line 178 "parser.y"
                             { (yyval.arg_type) = ARG_TYPE_CURVE; }
#line 1752 "parser.tab.c"
    break;

  case 39:
#line 179 "parser.y"
                             { (yyval.arg_type) = ARG_TYPE_IMAGE; }
#line 1758 "parser.tab.c"
    break;

  case 40:
#line 182 "parser.y"
                             { (yyval.exprtree) = (yyvsp[0].exprtree); }
#line 1764 "parser.tab.c"
    break;

  case 41:
#line 183 "parser.y"
                             { (yyval.exprtree) = (yyvsp[0].exprtree); }
#line 1770 "parser.tab.c"
    break;

  case 42:
#line 184 "parser.y"
                             { (yyval.exprtree) = make_var((yyvsp[0].ident)); free((yyvsp[0].ident)); }
#line 1776 "parser.tab.c"
    break;

  case 43:
#line 185 "parser.y"
                             { (yyval.exprtree) = make_tuple_exprtree((yyvsp[-1].exprtree)); }
#line 1782 "parser.tab.c"
    break;

  case 44:
#line 187 "parser.y"
                             {
				 (yyval.exprtree) = make_select(make_var((yyvsp[-3].ident)), make_tuple_exprtree((yyvsp[-1].exprtree)));
				 free((yyvsp[-3].ident));
			     }
#line 1791 "parser.tab.c"
    break;

  case 45:
#line 191 "parser.y"
                                         { (yyval.exprtree) = make_select((yyvsp[-4].exprtree), make_tuple_exprtree((yyvsp[-1].exprtree))); }
#line 1797 "parser.tab.c"
    break;

  case 46:
#line 192 "parser.y"
                             { (yyval.exprtree) = make_cast((yyvsp[-2].ident), (yyvsp[0].exprtree));
			       free((yyvsp[-2].ident)); }
#line 1804 "parser.tab.c"
    break;

  case 47:
#line 194 "parser.y"
                                { (yyval.exprtree) = make_convert((yyvsp[-2].ident), (yyvsp[0].exprtree)); free((yyvsp[-2].ident)); }
#line 1810 "parser.tab.c"
    break;

  case 48:
#line 195 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1816 "parser.tab.c"
    break;

  case 49:
#line 196 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1822 "parser.tab.c"
    break;

  case 50:
#line 197 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1828 "parser.tab.c"
    break;

  case 51:
#line 198 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1834 "parser.tab.c"
    break;

  case 52:
#line 199 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1840 "parser.tab.c"
    break;

  case 53:
#line 200 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1846 "parser.tab.c"
    break;

  case 54:
#line 201 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1852 "parser.tab.c"
    break;

  case 55:
#line 202 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1858 "parser.tab.c"
    break;

  case 56:
#line 203 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1864 "parser.tab.c"
    break;

  case 57:
#line 205 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1870 "parser.tab.c"
    break;

  case 58:
#line 207 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1876 "parser.tab.c"
    break;

  case 59:
#line 209 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1882 "parser.tab.c"
    break;

  case 60:
#line 210 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1888 "parser.tab.c"
    break;

  case 61:
#line 211 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1894 "parser.tab.c"
    break;

  case 62:
#line 212 "parser.y"
                             { (yyval.exprtree) = make_operator_function((yyvsp[-1].ident), (yyvsp[-2].exprtree), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1900 "parser.tab.c"
    break;

  case 63:
#line 214 "parser.y"
                             { (yyval.exprtree) = make_unary_operator_function((yyvsp[-1].ident), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1906 "parser.tab.c"
    break;

  case 64:
#line 216 "parser.y"
                             { (yyval.exprtree) = make_unary_operator_function((yyvsp[-1].ident), (yyvsp[0].exprtree)); free((yyvsp[-1].ident)); }
#line 1912 "parser.tab.c"
    break;

  case 65:
#line 217 "parser.y"
                             { (yyval.exprtree) = (yyvsp[-1].exprtree); }
#line 1918 "parser.tab.c"
    break;

  case 66:
#line 219 "parser.y"
                             { (yyval.exprtree) = make_function((yyvsp[-3].ident), (yyvsp[-1].exprtree));
			       free((yyvsp[-3].ident)); }
#line 1925 "parser.tab.c"
    break;

  case 67:
#line 221 "parser.y"
                             { (yyval.exprtree) = make_assignment((yyvsp[-2].ident), (yyvsp[0].exprtree));
			       free((yyvsp[-2].ident)); }
#line 1932 "parser.tab.c"
    break;

  case 68:
#line 224 "parser.y"
                             { (yyval.exprtree) = make_sub_assignment((yyvsp[-5].ident), make_tuple_exprtree((yyvsp[-3].exprtree)), (yyvsp[0].exprtree));
			       free((yyvsp[-5].ident)); }
#line 1939 "parser.tab.c"
    break;

  case 69:
#line 226 "parser.y"
                             { (yyval.exprtree) = make_sequence((yyvsp[-2].exprtree), (yyvsp[0].exprtree)); }
#line 1945 "parser.tab.c"
    break;

  case 70:
#line 228 "parser.y"
                             { (yyval.exprtree) = make_if_then((yyvsp[-3].exprtree), (yyvsp[-1].exprtree)); }
#line 1951 "parser.tab.c"
    break;

  case 71:
#line 230 "parser.y"
                             { (yyval.exprtree) = make_if_then_else((yyvsp[-5].exprtree),
								(yyvsp[-3].exprtree),
								(yyvsp[-1].exprtree)); }
#line 1959 "parser.tab.c"
    break;

  case 72:
#line 234 "parser.y"
                             { (yyval.exprtree) = make_while((yyvsp[-3].exprtree), (yyvsp[-1].exprtree)); }
#line 1965 "parser.tab.c"
    break;

  case 73:
#line 236 "parser.y"
                             { (yyval.exprtree) = make_do_while((yyvsp[-3].exprtree), (yyvsp[-1].exprtree)); }
#line 1971 "parser.tab.c"
    break;

  case 74:
#line 238 "parser.y"
                             {
				 check_for_start((yyvsp[0].exprtree));
				 (yyval.exprtree) = make_assignment((yyvsp[-2].ident), (yyvsp[0].exprtree));
			     }
#line 1980 "parser.tab.c"
    break;

  case 75:
#line 243 "parser.y"
                             {
				 (yyval.exprtree) = make_for((yyvsp[-8].ident), (yyvsp[-5].exprtree), (yyvsp[-6].exprtree), (yyvsp[-3].exprtree), (yyvsp[-1].exprtree));
				 free((yyvsp[-8].ident));
			     }
#line 1989 "parser.tab.c"
    break;

  case 76:
#line 249 "parser.y"
                             { (yyval.exprtree) = 0; }
#line 1995 "parser.tab.c"
    break;

  case 77:
#line 250 "parser.y"
                             { (yyval.exprtree) = (yyvsp[0].exprtree); }
#line 2001 "parser.tab.c"
    break;

  case 78:
#line 253 "parser.y"
                                   { (yyval.exprtree) = (yyvsp[0].exprtree); }
#line 2007 "parser.tab.c"
    break;

  case 79:
#line 254 "parser.y"
                                   { (yyval.exprtree) = exprlist_append((yyvsp[-2].exprtree), (yyvsp[0].exprtree)); }
#line 2013 "parser.tab.c"
    break;

  case 80:
#line 257 "parser.y"
                                       { (yyval.exprtree) = (yyvsp[0].exprtree); }
#line 2019 "parser.tab.c"
    break;

  case 81:
#line 258 "parser.y"
                                       { (yyval.exprtree) = exprlist_append((yyvsp[-2].exprtree), (yyvsp[0].exprtree)); }
#line 2025 "parser.tab.c"
    break;

  case 82:
#line 261 "parser.y"
                                   { (yyval.exprtree) = (yyvsp[0].exprtree); }
#line 2031 "parser.tab.c"
    break;

  case 83:
#line 262 "parser.y"
                                   { (yyval.exprtree) = make_range((yyvsp[-2].exprtree), (yyvsp[0].exprtree)); }
#line 2037 "parser.tab.c"
    break;


#line 2041 "parser.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 273 "parser.y"


int
yyerror (char *s)
{
    sprintf(error_string, _("Parse error."));
    error_region = scanner_last_token_region();
    JUMP(1);

    return 0;
}
