#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <gmodule.h>

#include "cgen.h"
#include "tags.h"
#include "builtins.h"
#include "overload.h"

GModule *module = 0;
mathfunc_t eval_c_code = 0;

void
enumerate_tmpvars (exprtree *tree, int *nextone, int force, FILE *out)
{
    if (force != -1)
	tree->tmpvarnum = force;
    else
    {
	int i;

	tree->tmpvarnum = (*nextone)++;
	for (i = 0; i < tree->result.length; ++i)
	    fprintf(out, "float tmpvar_%d_%d;\n", tree->tmpvarnum, i);
    }

    switch (tree->type)
    {
	case EXPR_TUPLE_CONST :
	case EXPR_INTERNAL :
	case EXPR_VARIABLE :
	    break;

	case EXPR_TUPLE :
	    {
		exprtree *elem;

		for (elem = tree->val.tuple.elems; elem != 0; elem = elem->next)
		    enumerate_tmpvars(elem, nextone, -1, out);
	    }
	    break;

	case EXPR_SELECT :
	    enumerate_tmpvars(tree->val.select.tuple, nextone, -1, out);
	    enumerate_tmpvars(tree->val.select.num, nextone, -1, out);
	    break;

	case EXPR_CAST :
	    enumerate_tmpvars(tree->val.cast.tuple, nextone, tree->tmpvarnum, out);
	    break;

	case EXPR_FUNC :
	    {
		exprtree *arg;

		for (arg = tree->val.func.args; arg != 0; arg = arg->next)
		    enumerate_tmpvars(arg, nextone, -1, out);
	    }
	    break;

	case EXPR_ASSIGNMENT :
	    enumerate_tmpvars(tree->val.assignment.value, nextone, -1, out);
	    break;

	case EXPR_SEQUENCE :
	    enumerate_tmpvars(tree->val.operator.left, nextone, -1, out);
	    enumerate_tmpvars(tree->val.operator.right, nextone, tree->tmpvarnum, out);
	    break;

	case EXPR_IF_THEN :
	    enumerate_tmpvars(tree->val.ifExpr.condition, nextone, -1, out);
	    enumerate_tmpvars(tree->val.ifExpr.consequent, nextone, tree->tmpvarnum, out);
	    break;

	case EXPR_IF_THEN_ELSE :
	    enumerate_tmpvars(tree->val.ifExpr.condition, nextone, -1, out);
	    enumerate_tmpvars(tree->val.ifExpr.consequent, nextone, tree->tmpvarnum, out);
	    enumerate_tmpvars(tree->val.ifExpr.alternative, nextone, tree->tmpvarnum, out);
	    break;

	case EXPR_WHILE :
	case EXPR_DO_WHILE :
	    enumerate_tmpvars(tree->val.whileExpr.invariant, nextone, -1, out);
	    enumerate_tmpvars(tree->val.whileExpr.body, nextone, -1, out);
	    break;
    }
}

void
gen_c_code_recursive (exprtree *tree, FILE *out)
{
    int i;

    switch (tree->type)
    {
	case EXPR_TUPLE_CONST :
	    for (i = 0; i < tree->val.tuple_const.length; ++i)
		fprintf(out, "tmpvar_%d_%d = %f;\n", tree->tmpvarnum, i, tree->val.tuple_const.data[i]);
	    break;

	case EXPR_TUPLE :
	    {
		exprtree *elem;

		for (i = 0, elem = tree->val.tuple.elems; elem != 0; ++i, elem = elem->next)
		{
		    gen_c_code_recursive(elem, out);
		    fprintf(out, "tmpvar_%d_%d = tmpvar_%d_0;\n", tree->tmpvarnum, i, elem->tmpvarnum);
		}
	    }
	    break;

	case EXPR_SELECT :
	    gen_c_code_recursive(tree->val.select.tuple, out);
	    gen_c_code_recursive(tree->val.select.num, out);
	    for (i = 1; i < tree->val.select.tuple->result.length; ++i)
		fprintf(out,
			"if (tmpvar_%d_0 < %d)\n"
			"    tmpvar_%d_0 = tmpvar_%d_%d;\n"
			"else ",
			tree->val.select.num->tmpvarnum, i,
			tree->tmpvarnum, tree->val.select.tuple->tmpvarnum, i - 1);
	    fprintf(out, "tmpvar_%d_0 = tmpvar_%d_%d;\n",
		    tree->tmpvarnum, tree->val.select.tuple->tmpvarnum,
		    tree->val.select.tuple->result.length - 1);
	    break;

	case EXPR_CAST :
	    gen_c_code_recursive(tree->val.cast.tuple, out);
	    break;

	case EXPR_INTERNAL :
	    fprintf(out,
		    "{\n"
		    "tuple_t *tuple = (tuple_t*)%p;\n",
		    &tree->val.internal->value);
	    for (i = 0; i < tree->result.length; ++i)
		fprintf(out, "tmpvar_%d_%d = tuple->data[%d];\n", tree->tmpvarnum, i, i);
	    fprintf(out, "}\n");
	    break;

	case EXPR_FUNC :
	    {
		exprtree *arg;
		int numargs = 0;
		int *invarnums, *invarlengths;

		for (arg = tree->val.func.args; arg != 0; arg = arg->next)
		{
		    gen_c_code_recursive(arg, out);
		    ++numargs;
		}

		invarnums = (int*)malloc(numargs * sizeof(int));
		invarlengths = (int*)malloc(numargs * sizeof(int));

		for (i = 0, arg = tree->val.func.args; arg != 0; ++i, arg = arg->next)
		{
		    invarnums[i] = arg->tmpvarnum;
		    invarlengths[i] = arg->result.length;
		}

		fprintf(out, "{\n");
		tree->val.func.entry->v.builtin.generator(out, invarnums, invarlengths,
							  tree->tmpvarnum);
		fprintf(out, "}\n");
	    }
	    break;

	case EXPR_VARIABLE :
	    for (i = 0; i < tree->result.length; ++i)
		fprintf(out, "tmpvar_%d_%d = uservar_%s_%d;\n",
			tree->tmpvarnum, i, tree->val.var->name, i);
	    break;

	case EXPR_ASSIGNMENT :
	    gen_c_code_recursive(tree->val.assignment.value, out);
	    for (i = 0; i < tree->result.length; ++i)
		fprintf(out, "uservar_%s_%d = tmpvar_%d_%d;\n",
			tree->val.assignment.var->name, i, tree->val.assignment.value->tmpvarnum, i);
	    break;

	case EXPR_SEQUENCE :
	    gen_c_code_recursive(tree->val.operator.left, out);
	    gen_c_code_recursive(tree->val.operator.right, out);
	    break;

	case EXPR_IF_THEN :
	    gen_c_code_recursive(tree->val.ifExpr.condition, out);
	    fprintf(out,
		    "if (tmpvar_%d_0 != 0.0)\n"
		    "{\n",
		    tree->val.ifExpr.condition->tmpvarnum);
	    gen_c_code_recursive(tree->val.ifExpr.consequent, out);
	    fprintf(out,
		    "}\n"
		    "else\n"
		    "{\n");
	    for (i = 0; i < tree->result.length; ++i)
		fprintf(out, "    tmpvar_%d_%d = 0.0;\n", tree->tmpvarnum, i);
	    fprintf(out, "}\n");
	    break;

	case EXPR_IF_THEN_ELSE :
	    gen_c_code_recursive(tree->val.ifExpr.condition, out);
	    fprintf(out,
		    "if (tmpvar_%d_0 != 0.0)\n"
		    "{\n",
		    tree->val.ifExpr.condition->tmpvarnum);
	    gen_c_code_recursive(tree->val.ifExpr.consequent, out);
	    fprintf(out,
		    "}\n"
		    "else\n"
		    "{\n");
	    gen_c_code_recursive(tree->val.ifExpr.alternative, out);
	    fprintf(out, "}\n");
	    break;

	case EXPR_WHILE :
	    fprintf(out,
		    "while (1)\n"
		    "{\n");
	    gen_c_code_recursive(tree->val.whileExpr.invariant, out);
	    fprintf(out,
		    "if (tmpvar_%d_0 == 0.0)\n"
		    "    break;\n",
		    tree->val.whileExpr.invariant->tmpvarnum);
	    gen_c_code_recursive(tree->val.whileExpr.body, out);
	    fprintf(out,
		    "}\n"
		    "tmpvar_%d_0 = 0.0;\n",
		    tree->tmpvarnum);
	    break;

	case EXPR_DO_WHILE :
	    fprintf(out,
		    "do\n"
		    "{\n");
	    gen_c_code_recursive(tree->val.whileExpr.body, out);
	    gen_c_code_recursive(tree->val.whileExpr.invariant, out);
	    fprintf(out,
		    "} while (tmpvar_%d_0 != 0.0);\n"
		    "tmpvar_%d_0 = 0.0;\n",
		    tree->val.whileExpr.invariant->tmpvarnum,
		    tree->tmpvarnum);
	    break;

	default :
	    assert(0);
    }
}

gboolean
gen_and_load_c_code (exprtree *tree)
{
    FILE *out;
    int numtmpvars = 0, i;
    variable_t *var;

    if (module != 0)
    {
	g_module_close(module);
	module = 0;
    }

    out = fopen("/tmp/mathfunc.c", "w");
    assert(out != 0);

    fprintf(out,
	    "#include <math.h>\n"
	    "void getOrigValIntersamplePixel(float,float,unsigned char*);\n"
	    "void getOrigValPixel(float,float,unsigned char*);\n"
	    "typedef struct\n"
	    "{\n"
	    "    float data[%d];\n"
	    "    int number;\n"
	    "    int length;\n"
	    "} tuple_t;\n"
	    "extern double user_curve_values[];\n"
	    "extern int user_curve_points;\n"
	    "extern tuple_t gradient_samples[];\n"
	    "extern int num_gradient_samples;\n"
	    "typedef void (*builtin_function_t) (void*);\n"
	    "extern tuple_t stack[];\n"
	    "extern int stackp;\n\n", MAX_TUPLE_LENGTH);
    fprintf(out,
	    "tuple_t* mathmapfunc (void)\n"
	    "{\n"
	    "int dummy;\n");

    for (var = firstVariable; var != 0; var = var->next)
	for (i = 0; i < var->type.length; ++i)
	    fprintf(out, "float uservar_%s_%d;\n", var->name, i);

    enumerate_tmpvars(tree, &numtmpvars, -1, out);
    gen_c_code_recursive(tree, out);

    for (i = 0; i < tree->result.length; ++i)
	fprintf(out,
		"stack[0].data[%d] = tmpvar_%d_%d;\n",
		i, tree->tmpvarnum, i);

    fprintf(out,
	    "stack[0].length = 4;\n"
	    "return &stack[0];\n"
	    "}\n");

    fclose(out);

    system("gcc -O9 -g -c -fPIC -o /tmp/mathfunc.o /tmp/mathfunc.c");
    system("gcc -shared -o /tmp/mathfunc.so /tmp/mathfunc.o");

    module = g_module_open("/tmp/mathfunc.so", 0);
    if (module == 0)
    {
	fprintf(stderr, "could not load module: %s\n", g_module_error());
	assert(0);
    }

    assert(g_module_symbol(module, "mathmapfunc", (void**)&eval_c_code));

    return TRUE;
}
