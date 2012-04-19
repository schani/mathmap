/*
 * cc.c
 *
 * MathMap
 *
 * Copyright (C) 2002-2009 Mark Probst
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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <complex.h>

#include <gmodule.h>

#include "../compiler-internals.h"
#include "../compiler_types.h"

static filter_code_t **filter_codes;

// defined in compiler-types.h
MAKE_TYPE_C_TYPE_NAME

/*** c code output ***/

static void
print_curve (curve_t *curve)
{
    g_assert_not_reached();
}

static void
print_gradient (gradient_t *gradient)
{
    g_assert_not_reached();
}

static void
print_image (image_t *image)
{
    g_assert_not_reached();
}

static void
print_tuple (float *tuple)
{
    g_assert_not_reached();
}

static void
print_tree_vector (tree_vector_t *tree_vector)
{
    g_assert_not_reached();
}

static void
output_value_name (FILE *out, value_t *value, int for_decl)
{
    if (value->index < 0)
    {
	if (value->compvar->type == TYPE_IMAGE)
	    fputs("UNINITED_IMAGE /* uninitialized */ ", out);
	else
	    fputs("0 /* uninitialized */ ", out);
    }
    else
    {
#ifndef NO_CONSTANTS_ANALYSIS
	if (!for_decl && compiler_is_permanent_const_value(value))
	{
	    if ((value->const_type | CONST_T) == (CONST_X | CONST_Y | CONST_T))
		fprintf(out, "xy_vars->");
	    else if ((value->const_type | CONST_T) == (CONST_Y | CONST_T))
		fprintf(out, "y_vars->");
	}
#endif

	if (value->compvar->var != 0)
	    fprintf(out, "var_%d_%d_%d", value->compvar->index, value->compvar->n, value->index);
	else
	    fprintf(out, "tmp_%d_%d", value->compvar->temp->number, value->index);
    }
}

static void
output_value_decl (FILE *out, value_t *value)
{
    if (!value->have_defined && value->index >= 0)
    {
	fprintf(out, "%s ", type_c_type_name(value->compvar->type));
	output_value_name(out, value, 1);
	fputs(";\n", out);
	value->have_defined = 1;
    }
}

static void
output_primary (FILE *out, primary_t *primary)
{
    switch (primary->kind)
    {
	case PRIMARY_VALUE :
	    output_value_name(out, primary->v.value, 0);
	    break;

	case PRIMARY_CONST :
	    switch (primary->const_type)
	    {
		TYPE_C_PRINTER

		default :
		    g_assert_not_reached();
	    }
	    break;

	default :
	    g_assert_not_reached();
    }
}

static char*
userval_element_name (userval_info_t *info)
{
    switch (info->type)
    {
	case USERVAL_INT_CONST :
	    return "int_const";
	case USERVAL_FLOAT_CONST :
	    return "float_const";
	case USERVAL_BOOL_CONST :
	    return "bool_const";
	case USERVAL_COLOR :
	    return "color.value";
	case USERVAL_CURVE :
	    return "curve";
	case USERVAL_GRADIENT :
	    return "gradient";
	case USERVAL_IMAGE :
	    return "image";
	default :
	    g_assert_not_reached();
    }
}

static void
output_make_mathmap_filter_closure (FILE *out, const char *var_name,
				    filter_t *filter, primary_t *args)
{
    int num_args = compiler_num_filter_args(filter) - 3;
    int i;
    userval_info_t *info;

    g_assert(filter->kind == FILTER_MATHMAP);

    fprintf(out,
	    "({ image_t *%s = ALLOC_CLOSURE_IMAGE(%d);"
	    "%s->v.closure.pools = pools;"
	    "%s->v.closure.xy_vars = 0;"
	    "%s->v.closure.funcs = &mathfuncs_%s;"
	    "%s->v.closure.func = filter_%s;",
	    var_name, num_args,
	    var_name,
	    var_name,
	    var_name, filter->name,
	    var_name, filter->name);

    for (i = 0, info = filter->userval_infos;
	 info != 0;
	 ++i, info = info->next)
    {
	fprintf(out, "CLOSURE_IMAGE_ARGS(%s)[%d].v.%s = ", var_name, i, userval_element_name(info));
	output_primary(out, &args[i]);
	fprintf(out, "; ");
    }
    g_assert(i == num_args);

    fprintf(out, "image->pixel_width = __canvasPixelW; image->pixel_height = __canvasPixelH;\n");
}

static void
output_rhs (FILE *out, rhs_t *rhs)
{
    switch (rhs->kind)
    {
	case RHS_PRIMARY :
	    output_primary(out, &rhs->v.primary);
	    break;

	case RHS_INTERNAL :
	    fputs(rhs->v.internal->name, out);
	    /* fprintf(out, "invocation->internals[%d].data[0]", rhs->v.internal->index); */
	    break;

	case RHS_OP :
	    {
		int i;

		fprintf(out, "%s(", rhs->v.op.op->name);
		for (i = 0; i < rhs->v.op.op->num_args; ++i)
		{
		    if (i > 0)
			fputs(",", out);
		    output_primary(out, &rhs->v.op.args[i]);
		}
		fputs(")", out);
	    }
	    break;

	case RHS_FILTER :
	    {
		int num_args = compiler_num_filter_args(rhs->v.filter.filter);

		output_make_mathmap_filter_closure(out, "image", rhs->v.filter.filter, rhs->v.filter.args);

		fprintf(out, "filter_%s(invocation, image, ", rhs->v.filter.filter->name);
		output_primary(out, &rhs->v.filter.args[num_args - 3]);
		fprintf(out, ", ");
		output_primary(out, &rhs->v.filter.args[num_args - 2]);
		fprintf(out, ", ");
		output_primary(out, &rhs->v.filter.args[num_args - 1]);
		fprintf(out, ", pools); })");
	    }
	    break;

	case RHS_CLOSURE :
	    {
		int i;
		userval_info_t *info;

		if (rhs->v.closure.filter->kind == FILTER_MATHMAP)
		{
		    output_make_mathmap_filter_closure(out, "image", rhs->v.closure.filter, rhs->v.closure.args);
		    fprintf(out, "image; })");
		}
		else if (rhs->v.closure.filter->kind == FILTER_NATIVE)
		{
		    int num_args = compiler_num_filter_args(rhs->v.closure.filter) - 3;

		    fprintf(out, "({ userval_t args[%d]; ", num_args);

		    for (i = 0, info = rhs->v.closure.filter->userval_infos;
			 info != 0;
			 ++i, info = info->next)
		    {
			fprintf(out, "args[%d].v.%s = ", i, userval_element_name(info));
			output_primary(out, &rhs->v.closure.args[i]);
			fprintf(out, "; ");
		    }
		    g_assert(i == num_args);

		    fprintf(out, "%s(invocation, args, pools); })", rhs->v.closure.filter->v.native.func_name);
		}
		else
		    g_assert_not_reached();
	    }
	    break;

	case RHS_TUPLE :
	    {
		int i;

		fprintf(out, "({ float *tuple = ALLOC_TUPLE(%d); ", rhs->v.tuple.length);

		for (i = 0; i < rhs->v.tuple.length; ++i)
		{
		    fprintf(out, "TUPLE_SET(tuple, %d, ", i);
		    output_primary(out, &rhs->v.tuple.args[i]);
		    fprintf(out, "); ");
		}

		fprintf(out, "tuple; })");
	    }
	    break;

	case RHS_TREE_VECTOR :
	    {
		int i;

		fprintf(out, "({ float tuple[%d]; ", rhs->v.tuple.length);

		for (i = 0; i < rhs->v.tuple.length; ++i)
		{
		    fprintf(out, "tuple[%d] = ", i);
		    output_primary(out, &rhs->v.tuple.args[i]);
		    fprintf(out, "; ");
		}
		fprintf(out, "; ALLOC_TREE_VECTOR(%d, tuple); })", rhs->v.tuple.length);
	    }
	    break;

	default :
	    g_assert_not_reached();
    }
}

static void
output_phis (FILE *out, statement_t *phis, int branch, unsigned int slice_flag)
{
    while (phis != 0)
    {
	rhs_t *rhs;

        if (phis->kind == STMT_NIL)
	    goto next;

#ifndef NO_CONSTANTS_ANALYSIS
	if (slice_flag != SLICE_IGNORE && (phis->slice_flags & slice_flag) == 0)
	    goto next;
#endif

	g_assert(slice_flag == SLICE_IGNORE || phis->kind == STMT_PHI_ASSIGN);

	rhs = ((branch == 0) ? phis->v.assign.rhs : phis->v.assign.rhs2);

	if (rhs->kind != RHS_PRIMARY
	    || rhs->v.primary.kind != PRIMARY_VALUE
	    || rhs->v.primary.v.value != phis->v.assign.lhs)
	{
	    output_value_name(out, phis->v.assign.lhs, 0);
	    fputs(" = ", out);
	    output_rhs(out, rhs);
	    fputs(";\n", out);
	}

    next:
	phis = phis->next;
    }
}

static void
output_stmts (FILE *out, statement_t *stmt, unsigned int slice_flag)
{
    while (stmt != 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	if (slice_flag == SLICE_IGNORE || (stmt->slice_flags & slice_flag))
#endif
	    switch (stmt->kind)
	    {
		case STMT_NIL :
#ifndef NO_CONSTANTS_ANALYSIS
		    g_assert(slice_flag == SLICE_IGNORE);
#endif
		    break;

		case STMT_ASSIGN :
		    output_value_name(out, stmt->v.assign.lhs, 0);
		    fputs(" = ", out);
		    output_rhs(out, stmt->v.assign.rhs);
		    fputs(";\n", out);
		    break;

		case STMT_PHI_ASSIGN :
		    g_assert_not_reached();
		    break;

		case STMT_IF_COND :
		    fputs("if (", out);
		    output_rhs(out, stmt->v.if_cond.condition);
		    fputs(")\n{\n", out);
		    output_stmts(out, stmt->v.if_cond.consequent, slice_flag);
		    output_phis(out, stmt->v.if_cond.exit, 0, slice_flag);
		    fputs("}\nelse\n{\n", out);
		    output_stmts(out, stmt->v.if_cond.alternative, slice_flag);
		    output_phis(out, stmt->v.if_cond.exit, 1, slice_flag);
		    fputs("}\n", out);
		    break;

		case STMT_WHILE_LOOP :
		    output_phis(out, stmt->v.while_loop.entry, 0, slice_flag);
		    fputs("while (", out);
		    output_rhs(out, stmt->v.while_loop.invariant);
		    fputs(")\n{\n", out);
		    output_stmts(out, stmt->v.while_loop.body, slice_flag);
		    output_phis(out, stmt->v.while_loop.entry, 1, slice_flag);
		    fputs("}\n", out);
		    break;

		default :
		    g_assert_not_reached();
	    }

	stmt = stmt->next;
    }
}

static void
_output_value_if_needed_decl (value_t *value, statement_t *stmt, void *info)
{
    CLOSURE_VAR(FILE*, out, 0);
    CLOSURE_VAR(int, const_type, 1);

    if ((value->const_type | CONST_T) == (const_type | CONST_T)
	&& compiler_is_permanent_const_value(value))
	output_value_decl(out, value);
}

static void
output_permanent_const_declarations (filter_code_t *code, FILE *out, int const_type)
{
    compiler_reset_have_defined(code->first_stmt);

    COMPILER_FOR_EACH_VALUE_IN_STATEMENTS(code->first_stmt, &_output_value_if_needed_decl, out, (void*)const_type);
}

static void
_output_value_if_needed_code (value_t *value, statement_t *stmt, void *info)
{
    CLOSURE_VAR(FILE*, out, 0);
    CLOSURE_VAR(int, const_type, 1);

    if ((compiler_is_temporary_const_value(value) || const_type == 0)
	 && (const_type == CONST_IGNORE || compiler_is_value_needed_for_const(value, const_type)))
	output_value_decl(out, value);
}

static void
output_permanent_const_code (filter_code_t *code, FILE *out, int const_type)
{
    unsigned int slice_flag = compiler_slice_flag_for_const_type(const_type);

    /* declarations */
    compiler_reset_have_defined(code->first_stmt);
    COMPILER_FOR_EACH_VALUE_IN_STATEMENTS(code->first_stmt, &_output_value_if_needed_code, out, (void*)const_type);

    /* code */
    compiler_slice_code_for_const(code->first_stmt, const_type);
    output_stmts(out, code->first_stmt, slice_flag);
}

static void
output_all_code (filter_code_t *code, FILE *out)
{
    COMPILER_FOR_EACH_VALUE_IN_STATEMENTS(code->first_stmt, &_output_value_if_needed_code, out, (void*)CONST_IGNORE);
    output_stmts(out, code->first_stmt, SLICE_IGNORE);
}

/*** template processing ***/

static char *include_path = 0;

static void
set_include_path (const char *path)
{
    if (include_path != 0)
	free(include_path);
    include_path = strdup(path);
    assert(include_path != 0);
}

static int
filter_template_processor (mathmap_t *mathmap, const char *directive, const char *arg, FILE *out, void *data)
{
    filter_code_t *code = (filter_code_t*)data;

    if (strcmp(directive, "g") == 0)
    {
#ifdef OPENSTEP
	putc('0', out);
#else
	putc('1', out);
#endif
    }
    else if (strcmp(directive, "name") == 0)
	fputs(code->filter->name, out);
    else if (strcmp(directive, "m") == 0)
	output_permanent_const_code(code, out, 0);
    else if (strcmp(directive, "xy_decls") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_declarations(code, out, CONST_X | CONST_Y);
#endif
    }
    else if (strcmp(directive, "x_decls") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_declarations(code, out, CONST_X);
#endif
    }
    else if (strcmp(directive, "y_decls") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_declarations(code, out, CONST_Y);
#endif
    }
    else if (strcmp(directive, "xy_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_code(code, out, CONST_X | CONST_Y);
#endif
    }
    else if (strcmp(directive, "x_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_code(code, out, CONST_X);
#endif
    }
    else if (strcmp(directive, "y_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_code(code, out, CONST_Y);
#endif
    }
    else if (strcmp(directive, "non_const_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_all_code(code, out);
#endif
    }
    else
	return 0;
    return 1;
}

int
compiler_template_processor (mathmap_t *mathmap, const char *directive, const char *arg, FILE *out, void *data)
{
    assert(mathmap->main_filter->v.mathmap.decl != NULL
	   && mathmap->main_filter->v.mathmap.decl->type == TOP_LEVEL_FILTER);

    if (strcmp(directive, "g") == 0)
    {
#ifdef OPENSTEP
	putc('0', out);
#else
	putc('1', out);
#endif
    }
    else if (strcmp(directive, "include") == 0)
    {
	fputs(include_path, out);
    }
    else if (strcmp(directive, "filter_name") == 0)
    {
	fprintf(out, "%s", mathmap->main_filter->name);
    }
    else if (strcmp(directive, "filter_docstring") == 0)
    {
	if (mathmap->main_filter->v.mathmap.decl->docstring != 0)
	    fprintf(out, "%s", mathmap->main_filter->v.mathmap.decl->docstring);
    }
    else if (strcmp(directive, "num_uservals") == 0)
    {
	fprintf(out, "%d", mathmap->main_filter->num_uservals);
    }
    else if (strcmp(directive, "native_filter_decls") == 0)
    {
	filter_t *filter;

	for (filter = mathmap->filters; filter != NULL; filter = filter->next)
	{
	    if (filter->kind != FILTER_NATIVE)
		continue;

	    fprintf(out, "DECLARE_NATIVE_FILTER(%s);\n", filter->v.native.func_name);
	}
    }
    else if (strcmp(directive, "filter_begin") == 0)
    {
	int i;
	filter_t *filter;

	g_assert(arg != 0);

	for (i = 0, filter = mathmap->filters;
	     filter != 0;
	     ++i, filter = filter->next)
	{
	    filter_code_t *code = filter_codes[i];

	    if (filter->kind != FILTER_MATHMAP)
		continue;

	    g_assert(code->filter == filter);

	    process_template(mathmap, arg, out, filter_template_processor, code);
	}
    }
    else
	return 0;
    return 1;
}

/*** compiling and loading/unloading ***/

static int
exec_cmd (char *log_filename, char *format, ...)
{
    va_list ap;
    char *cmdline, *full_cmdline;
    int result;
    FILE *log;

    va_start(ap, format);
    cmdline = g_strdup_vprintf(format, ap);
    va_end(ap);

    log = fopen(log_filename, "a");
    if (log != 0)
    {
	fprintf(log, "\n%s\n", cmdline);
	fclose(log);
    }

    full_cmdline = g_strdup_printf("%s >>%s 2>&1", cmdline, log_filename);

    result = system(full_cmdline);

    g_free(full_cmdline);
    g_free(cmdline);

    return result;
}

#ifdef OPENSTEP
#define	CGEN_CC		"cc -c -fPIC -faltivec -o"
#define	CGEN_LD		"cc -bundle -flat_namespace -undefined suppress -o"
#endif

#define TMP_PREFIX		"/tmp/mathfunc"

initfunc_t
gen_and_load_c_code (mathmap_t *mathmap, void **module_info, char *template_filename, char *include_path,
		     filter_code_t **the_filter_codes)
{
    static int last_mathfunc = 0;

    FILE *out;
    char *c_filename, *o_filename, *so_filename, *log_filename;
    int pid = getpid();
    initfunc_t initfunc;
#ifndef OPENSTEP
    void *initfunc_ptr;
    GModule *module = 0;
#endif

    filter_codes = the_filter_codes;

    c_filename = g_strdup_printf("%s%d_%d.c", TMP_PREFIX, pid, ++last_mathfunc);
    out = fopen(c_filename, "w");
    if (out == 0)
    {
	sprintf(error_string, _("Could not write temporary file `%s'"), c_filename);
	return 0;
    }

    set_include_path(include_path);
    if (!process_template_file(mathmap, template_filename, out, &compiler_template_processor, 0))
    {
	sprintf(error_string, _("Could not process template file `%s'"), template_filename);
	return 0;
    }

    filter_codes = 0;

    fclose(out);

    o_filename = g_strdup_printf("%s%d_%d.o", TMP_PREFIX, pid, last_mathfunc);
    log_filename = g_strdup_printf("%s%d_%d.log", TMP_PREFIX, pid, last_mathfunc);

    if (exec_cmd(log_filename, "%s %s %s", CGEN_CC, o_filename, c_filename) != 0)
    {
	sprintf(error_string, _("C compiler failed.  See logfile `%s'."), log_filename);
	return 0;
    }

    so_filename = g_strdup_printf("%s%d_%d.so", TMP_PREFIX, pid, last_mathfunc);

    if (exec_cmd(log_filename, "%s %s %s", CGEN_LD, so_filename, o_filename) != 0)
    {
	sprintf(error_string, _("Linker failed.  See logfile `%s'."), log_filename);
	return 0;
    }

#ifndef OPENSTEP
    module = g_module_open(so_filename, 0);
    if (module == 0)
    {
	sprintf(error_string, _("Could not load module `%s': %s."), so_filename, g_module_error());
	return 0;
    }

#ifdef DEBUG_OUTPUT
    printf("loaded %p\n", module);
#endif

    assert(g_module_symbol(module, "mathmapinit", &initfunc_ptr));
    initfunc = (initfunc_t)initfunc_ptr;

    *module_info = module;
#else
    {
        NSObjectFileImage objectFileImage;
        NSModule module;
        const char *moduleName = "Johnny";
        NSSymbol symbol;

        NSCreateObjectFileImageFromFile(so_filename, &objectFileImage);
	if (objectFileImage == 0)
	{
	    fprintf(stderr, "NSCreateObjectFileImageFromFile() failed\n");
	    return 0;
	}

        module = NSLinkModule(objectFileImage, moduleName,
			      NSLINKMODULE_OPTION_PRIVATE | NSLINKMODULE_OPTION_BINDNOW);
	if (module == 0)
	{
	    fprintf(stderr, "NSLinkModule() failed\n");
	    return 0;
	}
        NSDestroyObjectFileImage(objectFileImage);

	symbol = NSLookupSymbolInModule(module, "__init");
	if (symbol != 0)
	{
	    void (*init) (void) = NSAddressOfSymbol(symbol);
	    init();
	}

        symbol = NSLookupSymbolInModule(module, "_mathmapinit");
	assert(symbol != 0);
        initfunc = NSAddressOfSymbol(symbol);

	*module_info = module;
    }
#endif

#ifndef DONT_UNLINK_SO
    unlink(so_filename);
#endif
    g_free(so_filename);

    unlink(o_filename);
    g_free(o_filename);

#ifndef DONT_UNLINK_C
    unlink(c_filename);
#endif
    g_free(c_filename);

    unlink(log_filename);
    g_free(log_filename);

    return initfunc;
}

void
unload_c_code (void *module_info)
{
#ifndef OPENSTEP
    GModule *module = module_info;

#ifdef DEBUG_OUTPUT
    printf("unloading %p\n", module);
#endif

    assert(g_module_close(module));
#else
    NSModule module = module_info;
    bool success;

    success = NSUnLinkModule(module, NSUNLINKMODULE_OPTION_NONE);

    //printf("unloading of module: %d\n", success);
#endif
}

/*** plug-in generator ***/

#ifndef OPENSTEP
int
generate_plug_in (char *filter, char *output_filename,
		  char *template_filename, int analyze_constants,
		  template_processor_func_t template_processor)
{
    char template_path[strlen(TEMPLATE_DIR) + 1 + strlen(template_filename) + 1];
    FILE *out;
    mathmap_t *mathmap;

    sprintf(template_path, "%s/%s", TEMPLATE_DIR, template_filename);

    mathmap = parse_mathmap(filter);

    if (mathmap == 0)
    {
	fprintf(stderr, _("Error: %s\n"), error_string);
	return 0;
    }

    compiler_generate_ir_code(mathmap->main_filter, analyze_constants, 0, -1, FALSE);

    out = fopen(output_filename, "w");

    if (out == 0)
    {
	fprintf(stderr, _("Could not open output file `%s'\n"), output_filename);
	exit(1);
    }

    set_include_path(TEMPLATE_DIR);
    if (!process_template_file(mathmap, template_path, out, template_processor, 0))
    {
	fprintf(stderr, _("Could not process template file `%s'\n"), template_path);
	exit(1);
    }

    fclose(out);

    compiler_free_pools(mathmap);

    return 1;
}
#endif
