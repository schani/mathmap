/*
 * cgen.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2002 Mark Probst
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

#ifdef USE_CGEN

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#ifndef OPENSTEP
#include <gmodule.h>
#endif
#include <unistd.h>
#include <string.h>

#include "cgen.h"
#include "tags.h"
#include "builtins.h"
#include "overload.h"
#include "userval.h"
#include "mathmap.h"

#ifdef OPENSTEP
#include <mach-o/dyld.h>
#endif

void
enumerate_tmpvars (exprtree *tree, int *nextone, int force, FILE *out)
{
    if (force != -1)
	tree->tmpvarnum = force;
    else
    {
	tree->tmpvarnum = (*nextone)++;
	fprintf(out, "float tmpvar_%d[%d];\n", tree->tmpvarnum, tree->result.length);
    }

    switch (tree->type)
    {
	case EXPR_TUPLE_CONST :
	case EXPR_INTERNAL :
	case EXPR_VARIABLE :
	    break;

	case EXPR_USERVAL :
	    {
		exprtree *arg;

		for (arg = tree->val.userval.args; arg != 0; arg = arg->next)
		    enumerate_tmpvars(arg, nextone, -1, out);
	    }
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
	    enumerate_tmpvars(tree->val.select.subscripts, nextone, -1, out);
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
	    enumerate_tmpvars(tree->val.assignment.value, nextone, tree->tmpvarnum, out);
	    break;

	case EXPR_SUB_ASSIGNMENT :
	    enumerate_tmpvars(tree->val.sub_assignment.subscripts, nextone, -1, out);
	    enumerate_tmpvars(tree->val.sub_assignment.value, nextone, tree->tmpvarnum, out);
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
		fprintf(out, "tmpvar_%d[%d] = %f;\n", tree->tmpvarnum, i, tree->val.tuple_const.data[i]);
	    break;

	case EXPR_TUPLE :
	    {
		exprtree *elem;

		for (i = 0, elem = tree->val.tuple.elems; elem != 0; ++i, elem = elem->next)
		{
		    gen_c_code_recursive(elem, out);
		    fprintf(out, "tmpvar_%d[%d] = tmpvar_%d[0];\n", tree->tmpvarnum, i, elem->tmpvarnum);
		}
	    }
	    break;

	case EXPR_SELECT :
	    gen_c_code_recursive(tree->val.select.tuple, out);

	    if (tree->val.select.subscripts->type == EXPR_TUPLE_CONST)
	    {
		for (i = 0; i < tree->val.select.subscripts->result.length; ++i)
		{
		    int index = tree->val.select.subscripts->val.tuple_const.data[i];

		    if (index < 0 || index >= tree->val.select.tuple->result.length)
			fprintf(out, "tmpvar_%d[%d] = 0.0;\n",
				tree->tmpvarnum, i);
		    else
			fprintf(out, "tmpvar_%d[%d] = tmpvar_%d[%d];\n",
				tree->tmpvarnum, i,
				tree->val.select.tuple->tmpvarnum, index);
		}
	    }
	    else
	    {
		exprtree *elem;

		assert(tree->val.select.subscripts->type == EXPR_TUPLE);

		elem = tree->val.select.subscripts->val.tuple.elems;
		i = 0;
		while (elem != 0)
		{
		    if (elem->type == EXPR_TUPLE_CONST)
		    {
			int index = elem->val.tuple_const.data[0];

			if (index < 0 || index >= tree->val.select.tuple->result.length)
			    fprintf(out, "tmpvar_%d[%d] = 0.0;\n",
				    tree->tmpvarnum, i);
			else
			    fprintf(out, "tmpvar_%d[%d] = tmpvar_%d[%d];\n",
				    tree->tmpvarnum, i,
				    tree->val.select.tuple->tmpvarnum, index);
		    }
		    else
		    {
			gen_c_code_recursive(elem, out);
			fprintf(out,
				"{\n"
				"    int index = tmpvar_%d[0];\n"
				"\n"
				"    if (index < 0 || index >= %d)\n"
				"        tmpvar_%d[%d] = 0.0;\n"
				"    else\n"
				"        tmpvar_%d[%d] = tmpvar_%d[index];\n"
				"}\n",
				elem->tmpvarnum,
				tree->val.select.tuple->result.length,
				tree->tmpvarnum, i,
				tree->tmpvarnum, i, tree->val.select.tuple->tmpvarnum);
		    }

		    elem = elem->next;
		    ++i;
		}
	    }
	    break;

	case EXPR_CAST :
	    gen_c_code_recursive(tree->val.cast.tuple, out);
	    break;

	case EXPR_INTERNAL :
	    fprintf(out,
		    "{\n"
		    "tuple_t *tuple = &invocation->internals[%d];\n",
		    tree->val.internal->index);
	    for (i = 0; i < tree->result.length; ++i)
		fprintf(out, "tmpvar_%d[%d] = tuple->data[%d];\n", tree->tmpvarnum, i, i);
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
		/*
		tree->val.func.entry->v.builtin.generator(out, invarnums, invarlengths,
							  tree->tmpvarnum);
		*/
		fprintf(out, "}\n");
	    }
	    break;

	case EXPR_VARIABLE :
	    for (i = 0; i < tree->result.length; ++i)
		fprintf(out, "tmpvar_%d[%d] = uservar_%s[%d];\n",
			tree->tmpvarnum, i, tree->val.var->name, i);
	    break;

	case EXPR_USERVAL :
	    switch (tree->val.userval.info->type)
	    {
		case USERVAL_INT_CONST :
		    fprintf(out,
			    "tmpvar_%d[0] = invocation->uservals[%d].v.int_const;\n",
			    tree->tmpvarnum, tree->val.userval.info->index);
		    break;

		case USERVAL_FLOAT_CONST :
		    fprintf(out,
			    "tmpvar_%d[0] = invocation->uservals[%d].v.float_const;\n",
			    tree->tmpvarnum, tree->val.userval.info->index);
		    break;

		case USERVAL_BOOL_CONST :
		    fprintf(out,
			    "tmpvar_%d[0] = invocation->uservals[%d].v.bool_const;\n",
			    tree->tmpvarnum, tree->val.userval.info->index);
		    break;

		case USERVAL_CURVE :
		    gen_c_code_recursive(tree->val.userval.args, out);
		    fprintf(out,
			    "{\n"
			    "    int index = (int)(tmpvar_%d[0] * (%d - 1));\n"
			    "\n"
			    "    if (index < 0)\n"
			    "        index = 0;\n"
			    "    else if (index >= %d)\n"
			    "        index = %d - 1;\n"
			    "    tmpvar_%d[0] = invocation->uservals[%d].v.curve.values[index];\n"
			    "}\n",
			    tree->val.userval.args->tmpvarnum, USER_CURVE_POINTS,
			    USER_CURVE_POINTS,
			    USER_CURVE_POINTS,
			    tree->tmpvarnum, tree->val.userval.info->index);
		    break;

		case USERVAL_COLOR :
		    fprintf(out,
			    "{\n"
			    "    int i;\n"
			    "\n"
			    "    for (i = 0; i < 4; ++i)\n"
			    "        tmpvar_%d[i] = invocation->uservals[%d].v.color.value.data[i];\n"
			    "}\n",
			    tree->tmpvarnum, tree->val.userval.info->index);
		    break;

		case USERVAL_GRADIENT :
		    gen_c_code_recursive(tree->val.userval.args, out);
		    fprintf(out,
			    "{\n"
			    "    int index = (int)(tmpvar_%d[0] * (%d - 1)), i;\n"
			    "\n"
			    "    if (index < 0)\n"
			    "        index = 0;\n"
			    "    else if (index >= %d)\n"
			    "        index = %d - 1;\n"
			    "    for (i = 0; i < 4; ++i)\n"
			    "        tmpvar_%d[i] = invocation->uservals[%d].v.gradient.values[index][i];\n"
			    "}\n",
			    tree->val.userval.args->tmpvarnum, USER_GRADIENT_POINTS,
			    USER_GRADIENT_POINTS,
			    USER_GRADIENT_POINTS,
			    tree->tmpvarnum, tree->val.userval.info->index);
		    break;

		case USERVAL_IMAGE :
		    fprintf(out,
			    "tmpvar_%d[0] = invocation->uservals[%d].v.image.index;\n",
			    tree->tmpvarnum, tree->val.userval.info->index);
		    break;

		default :
		    assert(0);
	    }
	    break;

	case EXPR_ASSIGNMENT :
	    gen_c_code_recursive(tree->val.assignment.value, out);
	    for (i = 0; i < tree->result.length; ++i)
		fprintf(out, "uservar_%s[%d] = tmpvar_%d[%d];\n",
			tree->val.assignment.var->name, i, tree->val.assignment.value->tmpvarnum, i);
	    break;

	case EXPR_SUB_ASSIGNMENT :
	    gen_c_code_recursive(tree->val.sub_assignment.value, out);

	    if (tree->val.sub_assignment.subscripts->type == EXPR_TUPLE_CONST)
	    {
		for (i = 0; i < tree->result.length; ++i)
		{
		    int index = tree->val.sub_assignment.subscripts->val.tuple_const.data[i];

		    if (index >= 0 && index < tree->val.sub_assignment.var->type.length)
			fprintf(out, "uservar_%s[%d] = tmpvar_%d[%d];\n",
				tree->val.sub_assignment.var->name, index, tree->val.sub_assignment.value->tmpvarnum, i);
		}
	    }
	    else
	    {
		exprtree *elem;

		assert(tree->val.sub_assignment.subscripts->type == EXPR_TUPLE);

		elem = tree->val.sub_assignment.subscripts->val.tuple.elems;
		i = 0;
		while (elem != 0)
		{
		    if (elem->type == EXPR_TUPLE_CONST)
		    {
			int index = elem->val.tuple_const.data[0];

			if (index >= 0 && index < tree->val.sub_assignment.var->type.length)
			    fprintf(out, "uservar_%s[%d] = tmpvar_%d[%d];\n",
				    tree->val.sub_assignment.var->name, index, tree->val.sub_assignment.value->tmpvarnum, i);
		    }
		    else
		    {
			gen_c_code_recursive(elem, out);
			fprintf(out,
				"{\n"
				"    int index = tmpvar_%d[0];\n"
				"\n"
				"    if (index >= 0 || index < %d)\n"
				"        uservar_%s[index] = tmpvar_%d[%d];\n"
				"}\n",
				elem->tmpvarnum,
				tree->val.sub_assignment.var->type.length,
				tree->val.sub_assignment.var->name, tree->val.sub_assignment.value->tmpvarnum, i);
		    }

		    elem = elem->next;
		    ++i;
		}
	    }
	    break;

	case EXPR_SEQUENCE :
	    gen_c_code_recursive(tree->val.operator.left, out);
	    gen_c_code_recursive(tree->val.operator.right, out);
	    break;

	case EXPR_IF_THEN :
	    gen_c_code_recursive(tree->val.ifExpr.condition, out);
	    fprintf(out,
		    "if (tmpvar_%d[0] != 0.0)\n"
		    "{\n",
		    tree->val.ifExpr.condition->tmpvarnum);
	    gen_c_code_recursive(tree->val.ifExpr.consequent, out);
	    fprintf(out,
		    "}\n"
		    "else\n"
		    "{\n");
	    for (i = 0; i < tree->result.length; ++i)
		fprintf(out, "    tmpvar_%d[%d] = 0.0;\n", tree->tmpvarnum, i);
	    fprintf(out, "}\n");
	    break;

	case EXPR_IF_THEN_ELSE :
	    gen_c_code_recursive(tree->val.ifExpr.condition, out);
	    fprintf(out,
		    "if (tmpvar_%d[0] != 0.0)\n"
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
		    "if (tmpvar_%d[0] == 0.0)\n"
		    "    break;\n",
		    tree->val.whileExpr.invariant->tmpvarnum);
	    gen_c_code_recursive(tree->val.whileExpr.body, out);
	    fprintf(out,
		    "}\n"
		    "tmpvar_%d[0] = 0.0;\n",
		    tree->tmpvarnum);
	    break;

	case EXPR_DO_WHILE :
	    fprintf(out,
		    "do\n"
		    "{\n");
	    gen_c_code_recursive(tree->val.whileExpr.body, out);
	    gen_c_code_recursive(tree->val.whileExpr.invariant, out);
	    fprintf(out,
		    "} while (tmpvar_%d[0] != 0.0);\n"
		    "tmpvar_%d[0] = 0.0;\n",
		    tree->val.whileExpr.invariant->tmpvarnum,
		    tree->tmpvarnum);
	    break;

	default :
	    assert(0);
    }
}

#ifdef OPENSTEP
#define	MAX(a,b)	(((a)<(b))?(b):(a))
#define	CGEN_CC		"cc -c -o" 
#define	CGEN_LD		"cc -bundle -o"
#endif

initfunc_t
gen_and_load_c_code (mathmap_t *mathmap, void **module_info)
{
    FILE *template = fopen("template.c", "r");
    FILE *out;
    int numtmpvars = 0, i;
    variable_t *var;
    char *buf;
    int pid = getpid();
    int c;
    initfunc_t initfunc;
#ifndef OPENSTEP
    GModule *module = 0;
#endif

    assert(template != 0);

    buf = (char*)malloc(MAX(strlen(CGEN_CC), strlen(CGEN_LD)) + 512);

    sprintf(buf, "/tmp/mathfunc%d.c", pid);
    out = fopen(buf, "w");
    assert(out != 0);

    while ((c = fgetc(template)) != EOF)
    {
	if (c == '$')
	{
	    c = fgetc(template);
	    assert(c != EOF);

	    switch (c)
	    {
		case 'l' :
		    fprintf(out, "%d", MAX_TUPLE_LENGTH);
		    break;

		case 'c' :
#ifdef HAVE_COMPLEX
		    putc('1', out);
#else
		    putc('0', out);
#endif
		    break;

		case 'g' :
#ifdef GIMP
		    putc('1', out);
#else
		    putc('0', out);
#endif
		    break;

		case 'm' :
		    for (var = mathmap->variables; var != 0; var = var->next)
			fprintf(out, "float uservar_%s[%d];\n", var->name, var->type.length);

		    enumerate_tmpvars(mathmap->exprtree, &numtmpvars, -1, out);
		    gen_c_code_recursive(mathmap->exprtree, out);

		    for (i = 0; i < mathmap->exprtree->result.length; ++i)
			fprintf(out,
				"invocation->stack[0].data[%d] = tmpvar_%d[%d];\n",
				i, mathmap->exprtree->tmpvarnum, i);

		    fprintf(out,
			    "invocation->stack[0].length = 4;\n"
			    "return &invocation->stack[0];\n");
		    break;

		default :
		    putc(c, out);
		    break;
	    }
	}
	else
	    putc(c, out);
    }

    fclose(out);
    fclose(template);

    sprintf(buf, "%s /tmp/mathfunc%d.o /tmp/mathfunc%d.c", CGEN_CC, pid, pid);
    system(buf);

    sprintf(buf, "%s /tmp/mathfunc%d.so /tmp/mathfunc%d.o", CGEN_LD, pid, pid);
    system(buf);

    sprintf(buf, "/tmp/mathfunc%d.so", pid);

#ifndef OPENSTEP
    module = g_module_open(buf, 0);
    if (module == 0)
    {
	fprintf(stderr, "could not load module: %s\n", g_module_error());
	assert(0);
    }

    printf("loaded %lx\n", module);

    assert(g_module_symbol(module, "mathmapinit", (void**)&initfunc));

    unlink(buf);

    sprintf(buf, "/tmp/mathfunc%d.o", pid);
    unlink(buf);

    sprintf(buf, "/tmp/mathfunc%d.c", pid);
    unlink(buf);

    *module_info = module;
#else
    {
        NSObjectFileImage objectFileImage;
        NSModule module;
        const char *moduleName = "Johnny";
        NSSymbol symbol;
        
        NSCreateObjectFileImageFromFile(
            buf, &objectFileImage);

        module = NSLinkModule(
            objectFileImage, moduleName,
            NSLINKMODULE_OPTION_PRIVATE | NSLINKMODULE_OPTION_BINDNOW);
        NSDestroyObjectFileImage(objectFileImage);

        {
            symbol = NSLookupSymbolInModule(
                module, "__init");
            if (symbol) {
                void (*init) (void) = NSAddressOfSymbol(symbol);
                init();
            }
        }

        symbol = NSLookupSymbolInModule(
            module, "_mathmapinit");
        initfunc = NSAddressOfSymbol(symbol);

	*module_info = module;
    }
#endif

    free(buf);

    return initfunc;
}

void
unload_c_code (void *module_info)
{
#ifndef OPENSTEP
    GModule *module = module_info;

    printf("unloading %lx\n", module);
    assert(g_module_close(module));
#else
    /* FIXME */
#endif
}
#endif
