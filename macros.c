#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "postfix.h"
#include "internals.h"
#include "tags.h"
#include "exprtree.h"
#include "overload.h"

#include "macros.h"

static var_macro_t *first = 0;

void
register_variable_macro (const char *name,  macro_function_t function, tuple_info_t info)
{
    var_macro_t *macro = (var_macro_t*)malloc(sizeof(var_macro_t));

    strncpy(macro->name, name, MAX_MACRO_LENGTH);
    macro->name[MAX_MACRO_LENGTH] = '\0';
    macro->info = info;
    macro->function = function;

    macro->next = first;
    first = macro;
}

macro_function_t
lookup_variable_macro (const char *name, tuple_info_t *info)
{
    var_macro_t *macro;

    for (macro = first; macro != 0; macro = macro->next)
	if (strcmp(macro->name, name) == 0)
	{
	    *info = macro->info;
	    return macro->function;
	}

    return 0;
}

exprtree*
macro_var_x (exprtree *args)
{
    return make_select(make_var("xy"), make_number(0));
}

exprtree*
macro_var_y (exprtree *args)
{
    return make_select(make_var("xy"), make_number(1));
}

exprtree*
macro_var_r (exprtree *args)
{
    return make_select(make_var("ra"), make_number(0));
}

exprtree*
macro_var_a (exprtree *args)
{
    return make_select(make_var("ra"), make_number(1));
}

exprtree*
macro_var_big_x (exprtree *args)
{
    return make_select(make_var("XY"), make_number(0));
}

exprtree*
macro_var_big_y (exprtree *args)
{
    return make_select(make_var("XY"), make_number(1));
}

exprtree*
macro_var_big_w (exprtree *args)
{
    return make_select(make_var("WH"), make_number(0));
}

exprtree*
macro_var_big_h (exprtree *args)
{
    return make_select(make_var("WH"), make_number(1));
}

exprtree*
macro_var_pi (exprtree *args)
{
    return make_number(M_PI);
}

exprtree*
macro_func_origVal (exprtree *args)
{
    return make_function("origVal", make_function("toXY", args));
}

exprtree*
macro_func_origValXY (exprtree *args)
{
    return make_function("origVal", make_cast("xy", make_tuple(args)));
}

exprtree*
macro_func_origValRA (exprtree *args)
{
    return make_function("origVal", make_cast("ra", make_tuple(args)));
}

exprtree*
macro_func_red (exprtree *arg)
{
    return make_select(arg, make_number(0));
}

exprtree*
macro_func_green (exprtree *arg)
{
    return make_select(arg, make_number(1));
}

exprtree*
macro_func_blue (exprtree *arg)
{
    return make_select(arg, make_number(2));
}

exprtree*
macro_func_alpha (exprtree *arg)
{
    return make_select(arg, make_number(3));
}

exprtree*
macro_func_rgbColor (exprtree *args)
{
    return make_cast("rgba", make_tuple(exprlist_append(args, make_number(1.0))));
}

exprtree*
macro_func_rgbaColor (exprtree *args)
{
    return make_cast("rgba", make_tuple(args));
}

exprtree*
macro_func_grayColor (exprtree *arg)
{
    variable_t *tmpvar = new_temporary_variable(arg->result);

    return make_sequence(make_assignment(tmpvar->name, arg),
			 make_cast("rgba",
				   make_tuple(exprlist_append(make_var(tmpvar->name),
							      exprlist_append(make_var(tmpvar->name),
									      exprlist_append(make_var(tmpvar->name),
											      make_number(1.0)))))));
}

exprtree*
macro_func_grayaColor (exprtree *arg)
{
    variable_t *tmpvar = new_temporary_variable(arg->result);

    return make_sequence(make_assignment(tmpvar->name, arg),
			 make_cast("rgba",
				   make_tuple(exprlist_append(make_var(tmpvar->name),
							      exprlist_append(make_var(tmpvar->name),
									      exprlist_append(make_var(tmpvar->name),
											      arg->next))))));
}

exprtree*
macro_func_toXY (exprtree *arg)
{
    return arg;
}

exprtree*
macro_func_toRA (exprtree *arg)
{
    return arg;
}

exprtree*
macro_func_curve (exprtree *arg)
{
    return make_userval("user_curve", "curve", arg);
}

void
init_macros (void)
{
    register_variable_macro("x", macro_var_x, make_tuple_info(nil_tag_number, 1));
    register_variable_macro("y", macro_var_y, make_tuple_info(nil_tag_number, 1));
    register_variable_macro("r", macro_var_r, make_tuple_info(nil_tag_number, 1));
    register_variable_macro("a", macro_var_a, make_tuple_info(nil_tag_number, 1));
    register_variable_macro("X", macro_var_big_x, make_tuple_info(nil_tag_number, 1));
    register_variable_macro("Y", macro_var_big_y, make_tuple_info(nil_tag_number, 1));
    register_variable_macro("W", macro_var_big_w, make_tuple_info(nil_tag_number, 1));
    register_variable_macro("H", macro_var_big_h, make_tuple_info(nil_tag_number, 1));

    register_variable_macro("pi", macro_var_pi, make_tuple_info(nil_tag_number, 1));

    register_overloaded_macro("origVal", "((rgba 4) (ra 2))", macro_func_origVal);
    register_overloaded_macro("origValIntersample", "((rgba 4) (ra 2))", macro_func_origVal);
    register_overloaded_macro("origValXY", "((rgba 4) (T 1) (T 1))", macro_func_origValXY);
    register_overloaded_macro("origValRA", "((rgba 4) (T 1) (T 1))", macro_func_origValRA);

    register_overloaded_macro("red", "((nil 1) (rgba 4))", macro_func_red);
    register_overloaded_macro("green", "((nil 1) (rgba 4))", macro_func_green);
    register_overloaded_macro("blue", "((nil 1) (rgba 4))", macro_func_blue);
    register_overloaded_macro("alpha", "((nil 1) (rgba 4))", macro_func_alpha);

    register_overloaded_macro("red", "((nil 1) (rgba 4))", macro_func_red);
    register_overloaded_macro("green", "((nil 1) (rgba 4))", macro_func_green);
    register_overloaded_macro("blue", "((nil 1) (rgba 4))", macro_func_blue);
    register_overloaded_macro("alpha", "((nil 1) (rgba 4))", macro_func_alpha);

    register_overloaded_macro("rgbColor", "((rgba 4) (T 1) (T 1) (T 1))", macro_func_rgbColor);
    register_overloaded_macro("rgbaColor", "((rgba 4) (T 1) (T 1) (T 1) (T 1))", macro_func_rgbaColor);
    register_overloaded_macro("grayColor", "((rgba 4) (T 1))", macro_func_grayColor);
    register_overloaded_macro("grayaColor", "((rgba 4) (T 1) (T 1))", macro_func_grayaColor);

    register_overloaded_macro("toXY", "((xy 2) (xy 2))", macro_func_toXY);
    register_overloaded_macro("toRA", "((ra 2) (ra 2))", macro_func_toRA);

    register_overloaded_macro("curve", "((nil 1) (_ 1))", macro_func_curve);
}
