/*
 * compiler_test.c
 *
 * MathMap
 *
 * Copyright (C) 2002-2004 Mark Probst
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

#include "mathmap.h"

color_t
mathmap_get_pixel (mathmap_invocation_t *invocation, int drawable_index, int frame, int x, int y)
{
    return COLOR_BLACK;
}

#if 0
void
test_compiler (void)
{
    variable_t *vars = 0;

    compvar_t *a = make_variable(register_variable(&vars, "a", make_tuple_info(nil_tag_number, 1)), 0);
    compvar_t *t = make_temporary();
    compvar_t *b = make_variable(register_variable(&vars, "b", make_tuple_info(nil_tag_number, 1)), 0);

    emit_assign(make_lhs(a), make_int_const_rhs(0));
    emit_assign(make_lhs(b), make_int_const_rhs(1));
    emit_assign(make_lhs(t), make_compvar_rhs(a));
    emit_assign(make_lhs(a), make_compvar_rhs(b));
    emit_assign(make_lhs(b), make_compvar_rhs(t));
    emit_assign(make_lhs(a), make_op_rhs(OP_ADD, make_compvar_primary(a), make_compvar_primary(b)));

    start_if_cond(make_compvar_rhs(a));
    /* emit_assign(make_lhs(b), make_int_const_rhs(1)); */
    switch_if_branch();
    emit_assign(make_lhs(b), make_int_const_rhs(2));
    end_if_cond();

    start_while_loop(make_compvar_rhs(a));
    emit_assign(make_lhs(b), make_compvar_rhs(b));
    emit_assign(make_lhs(b), make_int_const_rhs(1));
    emit_assign(make_lhs(b), make_int_const_rhs(3));
    end_while_loop();

    dump_code(first_stmt, 0);
}
#endif

extern void init_internals (mathmap_t *mathmap);
extern void dump_code (statement_t *stmt, int indent);

void
test_mathmap_compiler (char *expr)
{
    static mathmap_t mathmap;
    FILE *template;
    /* static compvar_t *result[MAX_TUPLE_LENGTH]; */

    mathmap.variables = 0;
    mathmap.userval_infos = 0;
    mathmap.internals = 0;

    mathmap.exprtree = 0;

    init_internals(&mathmap);

    template = fopen("new_template.c", "r");
    assert(template != 0);

    the_mathmap = &mathmap;

    DO_JUMP_CODE {
	scanFromString(expr);
	yyparse();
	endScanningFromString();

	the_mathmap = 0;

	assert(mathmap.exprtree != 0);

	gen_and_load_c_code(&mathmap, 0, template);
	dump_code(first_stmt, 0);

	/*
	gen_code(mathmap.exprtree, result, 0);

	output_c_code();
	*/
    } WITH_JUMP_HANDLER {
	printf("%s\n", error_string);
	assert(0);
    } END_JUMP_HANDLER;
}

int
main (int argc, char *argv[])
{
    assert(argc == 2);

    init_tags();
    init_builtins();
    init_macros();
    init_compiler();

    /* test_compiler(); */
    test_mathmap_compiler(argv[1]);
    return 0;
}
