/*
 * llvm.c
 *
 * MathMap
 *
 * Copyright (C) 2009 Mark Probst
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

#include <iostream>
#include <map>

#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/PassManager.h>
#include <llvm/CallingConv.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Target/TargetData.h>
#include <llvm/ModuleProvider.h>
#include <llvm/TypeSymbolTable.h>

#include "../compiler-internals.h"
#include "../compiler_types.h"

using namespace std;
using namespace llvm;

struct compiler_error
{
    string info;
    compiler_error (string _info) { info = _info; }
};

class code_emitter
{
public:
    code_emitter (Module *module, filter_t *filter, filter_code_t *code);
    ~code_emitter ();

    // must be called in this order - the last one is optional
    void emit_init_frame_function ();
    void emit_filter_function ();
    void emit_main_filter_funcs ();

private:
    filter_t *filter;
    filter_code_t *filter_code;
    Module *module;

    Function *init_frame_function;
    Function *current_function;

    IRBuilder<> *builder;

    Value *invocation_arg;
    Value *frame_arg;
    Value *closure_arg;
    Value *pools_arg;

    StructType *x_vars_type;
    StructType *y_vars_type;
    StructType *xy_vars_type;

    Value *x_vars_var;
    Value *y_vars_var;
    Value *xy_vars_var;
    Value *ret_var;

    Value *complex_copy_var;

    map<value_t*, Value*> value_map;
    map<string, Value*> internal_map;
    map<value_t*, int> const_value_index_map;
    int next_const_value_index;

    map<value_t*, Value*> saved_set_values;

    void set_value (value_t *value, Value *llvm_value, bool commit = true);
    void commit_set_const_value (value_t *value, Value *llvm_value);
    void commit_set_const_values ();
    Value* lookup_value (value_t *value);

    void set_internal (internal_t *internal, Value *llvm_value);
    Value* lookup_internal (internal_t *internal);
    Value* lookup_internal (const char *name);

    Value* promote (Value *val, int type);

    void alloc_complex_copy_var ();

    void build_const_value_info (value_t *value, statement_t *stmt, int const_type,
				 vector<const Type*> *struct_elems);
    static void _build_const_value_info (value_t *value, statement_t *stmt, void *info);
    StructType* build_const_value_infos (int const_type);

    Value* emit_const_value_addr (value_t *value);

    Value* emit_sizeof (Type *type);

    void emit_stmts (statement_t *stmt, unsigned int slice_flag);
    void emit_phi_rhss (statement_t *stmt, bool left, map<rhs_t*, Value*> *rhs_map, int slice_flag);
    void emit_phis (statement_t *stmt, BasicBlock *left_bb, BasicBlock *right_bb,
		    map<rhs_t*, Value*> &rhs_map, int slice_flag);
    Value* emit_rhs (rhs_t *rhs);
    Value* emit_primary (primary_t *primary, bool need_float = false);
    Value* emit_closure (filter_t *filter, primary_t *args);

    void set_internals_from_invocation (Value *invocation_arg);
    void setup_xy_vars_from_closure ();
    void set_xy_vars_from_frame ();

    void setup_filter_function (bool is_main_filter_function);
    void setup_init_frame_function ();
    Value* setup_init_x_or_y_function (string function_name, const char *internal_name, StructType *vars_type);
    void finish_function ();
};

static string
filter_prefix_name (filter_t *filter, const char *prefix)
{
    return string(prefix) + string("_") + string(filter->name);
}

static string
filter_function_name (filter_t *filter)
{
    return filter_prefix_name(filter, "filter");
}

static string
main_filter_function_name (filter_t *filter)
{
    return filter_prefix_name(filter, "main_filter");
}

static string
init_frame_function_name (filter_t *filter)
{
    return filter_prefix_name(filter, "init_frame");
}

static string
init_x_function_name (filter_t *filter)
{
    return filter_prefix_name(filter, "init_x");
}

static string
init_y_function_name (filter_t *filter)
{
    return filter_prefix_name(filter, "init_y");
}

static Function*
lookup_filter_function (Module *module, filter_t *filter)
{
    Function *func = module->getFunction(filter_function_name(filter));

    g_assert(func != NULL);

    return func;
}

static Function*
lookup_main_filter_function (Module *module, filter_t *filter)
{
    Function *func = module->getFunction(main_filter_function_name(filter));

    g_assert(func != NULL);

    return func;
}

static Function*
lookup_init_frame_function (Module *module, filter_t *filter)
{
    Function *func = module->getFunction(init_frame_function_name(filter));

    g_assert(func != NULL);

    return func;
}

static Function*
lookup_init_x_function (Module *module, filter_t *filter)
{
    Function *func = module->getFunction(init_x_function_name(filter));

    g_assert(func != NULL);

    return func;
}

static Function*
lookup_init_y_function (Module *module, filter_t *filter)
{
    Function *func = module->getFunction(init_y_function_name(filter));

    g_assert(func != NULL);

    return func;
}

static Value*
make_int_const (int x)
{
    return ConstantInt::get(Type::Int32Ty, x, true);
}

static Value*
make_float_const (float x)
{
    return ConstantFP::get(Type::FloatTy, x);
}

static const Type*
get_void_ptr_type ()
{
    return PointerType::getUnqual(Type::Int8Ty);
}

static const Type*
get_invocation_ptr_type (Module *module)
{
    const Type *invocation_type = module->getTypeByName(string("struct.mathmap_invocation_t"));
    g_assert(invocation_type);
    return PointerType::getUnqual(invocation_type);
}

static const Type*
get_frame_ptr_type (Module *module)
{
    const Type *frame_type = module->getTypeByName(string("struct.mathmap_frame_t"));
    g_assert(frame_type);
    return PointerType::getUnqual(frame_type);
}

static const Type*
get_slice_ptr_type (Module *module)
{
    const Type *slice_type = module->getTypeByName(string("struct._mathmap_slice_t"));
    g_assert(slice_type);
    return PointerType::getUnqual(slice_type);
}

static const Type*
get_pools_ptr_type (Module *module)
{
    const Type *pools_type = module->getTypeByName(string("struct.pools_t"));
    g_assert(pools_type);
    return PointerType::getUnqual(pools_type);
}

code_emitter::code_emitter (Module *_module, filter_t *_filter, filter_code_t *code)
{
    module = _module;
    filter_code = code;
    filter = _filter;

    x_vars_type = y_vars_type = xy_vars_type = NULL;
    x_vars_var = y_vars_var = xy_vars_var = NULL;

    init_frame_function = lookup_init_frame_function(module, filter);
    g_assert(init_frame_function);
}

code_emitter::~code_emitter ()
{
}

Value*
code_emitter::emit_const_value_addr (value_t *value)
{
    g_assert(const_value_index_map.find(value) != const_value_index_map.end());

    int index = const_value_index_map[value];
    vector<Value*> indices;

    indices.push_back(make_int_const(0));
    indices.push_back(make_int_const(index));

    Value *const_var = NULL;

    if ((value->const_type | CONST_T) == (CONST_X | CONST_Y | CONST_T))
	const_var = xy_vars_var;
    else if ((value->const_type | CONST_T) == (CONST_X | CONST_T))
	const_var = x_vars_var;
    else if ((value->const_type | CONST_T) == (CONST_Y | CONST_T))
	const_var = y_vars_var;
    else
	g_assert_not_reached();

    return builder->CreateGEP(const_var, indices.begin(), indices.end());
}

static void
set_value_name (Value *llvm_value, value_t *value)
{
    char *name = compiler_get_value_name(value);

    llvm_value->setName(name);

    g_free(name);
}

void
code_emitter::set_value (value_t *value, Value *llvm_value, bool commit)
{
    g_assert(value);
    g_assert(llvm_value);
    g_assert(value_map.find(value) == value_map.end());
    g_assert(value->index >= 0);

    if (compiler_is_permanent_const_value(value))
    {
	if (commit)
	    commit_set_const_value(value, llvm_value);
	else
	    saved_set_values[value] = llvm_value;
    }
    else
    {
	value_map[value] = llvm_value;
	set_value_name(llvm_value, value);
    }
}

void
code_emitter::commit_set_const_value (value_t *value, Value *llvm_value)
{
    Value *addr = emit_const_value_addr(value);
    llvm_value = promote(llvm_value, value->compvar->type);

#ifdef DEBUG_OUTPUT
    printf("storing value ");
    compiler_print_value(value);
    printf(" with llvm value ");
    llvm_value->dump();
    printf(" at addr ");
    addr->dump();
#endif

    // FIXME: put this into value_map, too
    builder->CreateStore(llvm_value, addr);
    set_value_name(llvm_value, value);
}

void
code_emitter::commit_set_const_values ()
{
    map<value_t*, Value*>::iterator iter;

    for (iter = saved_set_values.begin(); iter != saved_set_values.end(); ++iter)
	commit_set_const_value((*iter).first, (*iter).second);

    saved_set_values.clear();
}

Value*
code_emitter::lookup_value (value_t *value)
{
    g_assert(value->index >= 0);

    if (compiler_is_permanent_const_value(value))
	// FIXME: put this into value_map again so that it only has to
	// be fetched once.
	return builder->CreateLoad(emit_const_value_addr(value));
    else
    {
	g_assert(value_map.find(value) != value_map.end());
	return value_map[value];
    }
}

void
code_emitter::set_internal (internal_t *internal, Value *llvm_value)
{
    g_assert(internal);
    g_assert(llvm_value);
    g_assert(internal_map.find(string(internal->name)) == internal_map.end());
    internal_map[string(internal->name)] = llvm_value;
    llvm_value->setName(internal->name);
}

Value*
code_emitter::lookup_internal (const char *internal_name)
{
    string name(internal_name);

    g_assert(internal_map.find(name) != internal_map.end());

    return internal_map[name];
}

Value*
code_emitter::lookup_internal (internal_t *internal)
{
    return lookup_internal(internal->name);
}

Value*
code_emitter::promote (Value *val, int type)
{
    switch (type)
    {
	case TYPE_FLOAT :
	    if (val->getType() == Type::Int32Ty)
		val = builder->CreateCall(module->getFunction(string("promote_int_to_float")), val);
	    else
		assert(val->getType() == Type::FloatTy);
	    break;

	case TYPE_COMPLEX :
	    if (val->getType() == Type::Int32Ty)
		val = builder->CreateCall(module->getFunction(string("promote_int_to_complex")), val);
	    else if (val->getType() == Type::FloatTy)
		val = builder->CreateCall(module->getFunction(string("promote_float_to_complex")), val);
	    break;
    }
    return val;
}

Value*
code_emitter::emit_primary (primary_t *primary, bool need_float)
{
    switch (primary->kind)
    {
	case PRIMARY_VALUE :
	    if (primary->v.value->index < 0)
	    {
		switch (primary->v.value->compvar->type)
		{
		    case TYPE_INT :
			return make_int_const(0);
		    case TYPE_FLOAT :
			return make_float_const(0.0);
		    case TYPE_IMAGE :
			return builder->CreateCall(module->getFunction(string("get_uninited_image")));
		    default :
			g_assert_not_reached();
		}
	    }
	    else
	    {
		Value *val = lookup_value(primary->v.value);

		if (need_float)
		    val = promote(val, TYPE_FLOAT);
		return val;
	    }

	case PRIMARY_CONST :
	    switch (primary->const_type) {
		case TYPE_INT :
		    if (need_float)
			return make_float_const((float)primary->v.constant.int_value);
		    else
			return make_int_const(primary->v.constant.int_value);
		case TYPE_FLOAT :
		    return make_float_const(primary->v.constant.float_value);
		case TYPE_COMPLEX :
		    assert(!need_float);
		    return builder->CreateCall2(module->getFunction(string("make_complex")),
						make_float_const(creal(primary->v.constant.complex_value)),
						make_float_const(cimag(primary->v.constant.complex_value)));
		case TYPE_COLOR :
		    assert(!need_float);
		    return builder->CreateCall4(module->getFunction(string("make_color")),
						make_int_const(RED(primary->v.constant.color_value)),
						make_int_const(GREEN(primary->v.constant.color_value)),
						make_int_const(BLUE(primary->v.constant.color_value)),
						make_int_const(ALPHA(primary->v.constant.color_value)));
		default :
		    g_assert_not_reached();
	    }

	default:
	    g_assert_not_reached();
    }
}

static const char*
get_userval_set_func_name (int type)
{
    switch (type)
    {
	case USERVAL_INT_CONST :
	    return "set_userval_int";
	case USERVAL_FLOAT_CONST :
	    return "set_userval_float";
	case USERVAL_BOOL_CONST :
	    return "set_userval_bool";
	case USERVAL_COLOR :
	    return "set_userval_color";
	case USERVAL_CURVE :
	    return "set_userval_curve";
	case USERVAL_GRADIENT :
	    return "set_userval_gradient";
	case USERVAL_IMAGE :
	    return "set_userval_image";
	default :
	    g_assert_not_reached();
    }
}

Value*
code_emitter::emit_closure (filter_t *closure_filter, primary_t *args)
{
    int num_args = compiler_num_filter_args(closure_filter) - 3;
    Value *closure = NULL, *uservals;
    bool have_size;
    userval_info_t *info;
    int i;

    g_assert(closure_filter->kind == FILTER_MATHMAP || closure_filter->kind == FILTER_NATIVE);

    if (closure_filter->kind == FILTER_MATHMAP)
    {
	closure = builder->CreateCall4(module->getFunction(string("alloc_closure_image")),
				       invocation_arg, pools_arg, make_int_const(num_args),
				       lookup_filter_function(module, closure_filter));
	uservals = builder->CreateCall(module->getFunction(string("get_closure_uservals")), closure);
    }
    else
	uservals = builder->CreateCall2(module->getFunction(string("alloc_uservals")),
					pools_arg,
					make_int_const(compiler_num_filter_args(closure_filter) - 3));

    have_size = FALSE;
    for (i = 0, info = closure_filter->userval_infos;
	 info != 0;
	 ++i, info = info->next)
    {
	const char *set_func_name = get_userval_set_func_name(info->type);
	Value *arg = emit_primary(&args[i]);

	/* FIXME: remove this eventually - bool needs to be an int */
	if (info->type == USERVAL_BOOL_CONST)
	    arg = promote(arg, TYPE_FLOAT);

	builder->CreateCall3(module->getFunction(string(set_func_name)), uservals, make_int_const(i), arg);

	if (closure_filter->kind == FILTER_MATHMAP
	    && info->type == USERVAL_IMAGE
	    && !have_size)
	{
	    builder->CreateCall2(module->getFunction(string("set_closure_size_from_image")), closure, arg);
	    have_size = true;
	}
    }
    g_assert(i == num_args);

    if (closure_filter->kind == FILTER_MATHMAP)
    {
	if (!have_size)
	    builder->CreateCall3(module->getFunction(string("set_closure_pixel_size")),
				 closure, lookup_internal("__canvasPixelW"), lookup_internal("__canvasPixelH"));

	return closure;
    }
    else
    {
	string filter_func_name = string("llvm_") + string(closure_filter->v.native.func_name);
	return builder->CreateCall3(module->getFunction(filter_func_name),
				    invocation_arg, uservals, pools_arg);
    }
}

static bool
is_complex_return_type (const Type *type)
{
    if (sizeof(gpointer) == 4)
	return type == Type::Int64Ty;
    else if (sizeof(gpointer) == 8)
    {
	static Type *ret_type;

	if (!ret_type)
	{
	    vector<const Type*> elems;

	    elems.push_back(Type::DoubleTy);

	    ret_type = StructType::get(elems);
	}

	g_assert(ret_type);

	return type == ret_type;
    }
    else
	g_assert_not_reached();
}

static const Type*
llvm_type_for_type (Module *module, type_t type)
{
    switch (type)
    {
	case TYPE_INT :
	    return Type::Int32Ty;
	case TYPE_FLOAT :
	    return Type::FloatTy;
	case TYPE_COMPLEX :
	    if (sizeof(gpointer) == 4)
	    {
		static const Type *result;

		vector<const Type*> elems;

		if (result)
		    return result;

		elems.push_back(Type::FloatTy);
		elems.push_back(Type::FloatTy);

		result = StructType::get(elems);

		g_assert(result != NULL);
		return result;
	    }
	    else if (sizeof(gpointer) == 8)
		return Type::DoubleTy;
	    else
		g_assert_not_reached();
	case TYPE_IMAGE :
	    return PointerType::getUnqual(module->getTypeByName(string("struct._image_t")));
	case TYPE_TUPLE :
	    return PointerType::getUnqual(Type::FloatTy);
	case TYPE_COLOR :
	    return Type::Int32Ty;
	case TYPE_CURVE :
	    return PointerType::getUnqual(module->getTypeByName(string("struct.curve_t")));
	case TYPE_GRADIENT :
	    return PointerType::getUnqual(module->getTypeByName(string("struct.gradient_t")));
	default :
	    g_assert_not_reached();
    }
}

void
code_emitter::alloc_complex_copy_var ()
{
    complex_copy_var = builder->CreateAlloca(llvm_type_for_type(module, TYPE_COMPLEX));
}

Value*
code_emitter::emit_rhs (rhs_t *rhs)
{
    switch (rhs->kind) {
	case RHS_PRIMARY :
	    return emit_primary(&rhs->v.primary);
	case RHS_INTERNAL :
	    return lookup_internal(rhs->v.internal);
	case RHS_OP :
	    {
		operation_t *op = rhs->v.op.op;
		type_t promotion_type = TYPE_NIL;
		char *function_name = compiler_function_name_for_op_rhs(rhs, &promotion_type);

		if (promotion_type == TYPE_NIL)
		    assert(op->type_prop == TYPE_PROP_CONST);
		if (op->type_prop != TYPE_PROP_CONST)
		    assert(promotion_type != TYPE_NIL);

		Function *func = module->getFunction(string(function_name));
		g_assert(func);
		vector<Value*> args;
		args.push_back(invocation_arg);
		args.push_back(closure_arg);
		args.push_back(pools_arg);
		for (int i = 0; i < rhs->v.op.op->num_args; ++i) {
		    type_t type = promotion_type == TYPE_NIL ? op->arg_types[i] : promotion_type;
		    Value *val = emit_primary(&rhs->v.op.args[i], type == TYPE_FLOAT);
		    val = promote(val, type);

#ifndef __MINGW32__
		    if (sizeof(gpointer) == 4 && val->getType() == llvm_type_for_type(module, TYPE_COMPLEX))
		    {
			Value *copy = complex_copy_var;
			builder->CreateStore(val, copy);
			val = copy;
		    }
#endif

#ifdef DEBUG_OUTPUT
		    val->dump();
#endif
		    args.push_back(val);
		}
#ifdef DEBUG_OUTPUT
		func->dump();
#endif
		Value *result = builder->CreateCall(func, args.begin(), args.end());
		/* FIXME: this is ugly - we should check for the type
		   of the operation or resulting value */
		if (is_complex_return_type(result->getType()))
		{
		    /* The result is complex, whose representation
		       differs between archs, and we need to transform
		       it into another arch-dependent
		       representation. */
		    if (sizeof(gpointer) == 4)
		    {
			Value *local = complex_copy_var;
			Value *local_ptr = builder->CreateBitCast(local, PointerType::getUnqual(Type::Int64Ty));
			builder->CreateStore(result, local_ptr);
			result = builder->CreateLoad(local);
		    }
		    else if (sizeof(gpointer) == 8)
			result = builder->CreateExtractValue(result, 0);
		    else
			g_assert_not_reached();
		}
		return result;
	    }

	case RHS_FILTER :
	    {
		int num_args = compiler_num_filter_args(rhs->v.filter.filter);
		Value *closure = emit_closure(rhs->v.filter.filter, rhs->v.filter.args);
		Function *func = lookup_filter_function(module, rhs->v.filter.filter);
		vector<Value*> args;

		args.push_back(invocation_arg);
		args.push_back(closure);
		args.push_back(emit_primary(&rhs->v.filter.args[num_args - 3]));
		args.push_back(emit_primary(&rhs->v.filter.args[num_args - 2]));
		args.push_back(emit_primary(&rhs->v.filter.args[num_args - 1]));
		args.push_back(pools_arg);

		return builder->CreateCall(func, args.begin(), args.end());
	    }

	case RHS_CLOSURE :
	    return emit_closure(rhs->v.closure.filter, rhs->v.closure.args);

	case RHS_TUPLE :
	    {
		Function *set_func = module->getFunction(string("tuple_set"));
		Value *tuple = builder->CreateCall2(module->getFunction(string("alloc_tuple")),
						    pools_arg,
						    make_int_const(rhs->v.tuple.length));
		int i;

		for (i = 0; i < rhs->v.tuple.length; ++i)
		{
		    Value *val = emit_primary(&rhs->v.tuple.args[i], true);
		    builder->CreateCall3(set_func, tuple, make_int_const(i), val);
		}
		return tuple;
	    }

	default :
	    g_assert_not_reached();
    }
}

static gboolean
must_emit_stmt (statement_t *stmt, int slice_flag)
{
    return slice_flag == SLICE_IGNORE || (stmt->slice_flags & slice_flag);
}

void
code_emitter::emit_phi_rhss (statement_t *stmt, bool left, map<rhs_t*, Value*> *rhs_map, int slice_flag)
{
    for (; stmt != NULL; stmt = stmt->next)
    {
	if (!must_emit_stmt(stmt, slice_flag))
	    continue;

	switch (stmt->kind)
	{
	    case STMT_NIL :
		g_assert(slice_flag == SLICE_IGNORE);
		break;

	    case STMT_PHI_ASSIGN :
		{
		    int type = stmt->v.assign.lhs->compvar->type;
		    Value *val;
		    rhs_t *rhs = left ? stmt->v.assign.rhs : stmt->v.assign.rhs2;

		    val = emit_rhs(rhs);
		    val = promote(val, type);

		    (*rhs_map)[rhs] = val;
		}
		break;

	    default :
		g_assert_not_reached();
	}
    }
}

void
code_emitter::emit_phis (statement_t *stmt, BasicBlock *left_bb, BasicBlock *right_bb,
			 map<rhs_t*, Value*> &rhs_map, int slice_flag)
{
    for (; stmt != NULL; stmt = stmt->next)
    {
	if (!must_emit_stmt(stmt, slice_flag))
	    continue;

	switch (stmt->kind)
	{
	    case STMT_NIL :
		g_assert(slice_flag == SLICE_IGNORE);
		break;

	    case STMT_PHI_ASSIGN :
		{
		    Value *left = NULL;
		    Value *right = NULL;
		    int compvar_type = stmt->v.assign.lhs->compvar->type;
		    const Type *type = llvm_type_for_type(module, compvar_type);

		    if (left_bb)
		    {
			left = rhs_map[stmt->v.assign.rhs];
			g_assert(left != NULL);
		    }
		    if (right_bb)
		    {
			right = rhs_map[stmt->v.assign.rhs2];
			g_assert(right != NULL);
		    }

		    PHINode *phi;

		    if (left_bb)
		    {
			phi = builder->CreatePHI(type);
			phi->addIncoming(left, left_bb);
			set_value(stmt->v.assign.lhs, phi, false);
		    }
		    else
			phi = cast<PHINode>(lookup_value(stmt->v.assign.lhs));

		    if (right_bb)
			phi->addIncoming(right, right_bb);
		}
		break;

	    default:
		g_assert_not_reached();
	}
    }

    commit_set_const_values();
}

void
code_emitter::emit_stmts (statement_t *stmt, unsigned int slice_flag)
{
    for (; stmt != NULL; stmt = stmt->next)
    {
	if (!must_emit_stmt(stmt, slice_flag))
	    continue;

	switch (stmt->kind)
	{
	    case STMT_NIL :
		g_assert(slice_flag == SLICE_IGNORE);
		break;

	    case STMT_ASSIGN :
#ifdef DEBUG_OUTPUT
		compiler_print_assign_statement(stmt);
		printf("\n");
#endif
		if (stmt->v.assign.rhs->kind == RHS_OP
		    && stmt->v.assign.rhs->v.op.op->index == OP_OUTPUT_TUPLE)
		    builder->CreateRet(emit_primary(&stmt->v.assign.rhs->v.op.args[0]));
		else
		    set_value(stmt->v.assign.lhs, emit_rhs(stmt->v.assign.rhs));
		break;

	    case STMT_IF_COND :
		{
		    Value *condition_number = emit_rhs(stmt->v.if_cond.condition);
		    Value *condition;
		    map<rhs_t*, Value*> rhs_map;

		    if (condition_number->getType() == Type::Int32Ty)
			condition = builder->CreateICmpNE(condition_number, make_int_const(0));
		    else if (condition_number->getType() == Type::FloatTy)
			condition = builder->CreateFCmpONE(condition_number, make_float_const(0.0));
		    else
			g_assert_not_reached();

		    BasicBlock *then_bb = BasicBlock::Create("then", current_function);
		    BasicBlock *else_bb = BasicBlock::Create("else");
		    BasicBlock *merge_bb = BasicBlock::Create("ifcont");

		    builder->CreateCondBr(condition, then_bb, else_bb);

		    builder->SetInsertPoint(then_bb);
		    emit_stmts(stmt->v.if_cond.consequent, slice_flag);
		    emit_phi_rhss(stmt->v.if_cond.exit, true, &rhs_map, slice_flag);
		    builder->CreateBr(merge_bb);
		    then_bb = builder->GetInsertBlock();

		    current_function->getBasicBlockList().push_back(else_bb);
		    builder->SetInsertPoint(else_bb);
		    emit_stmts(stmt->v.if_cond.alternative, slice_flag);
		    emit_phi_rhss(stmt->v.if_cond.exit, false, &rhs_map, slice_flag);
		    builder->CreateBr(merge_bb);
		    else_bb = builder->GetInsertBlock();

		    current_function->getBasicBlockList().push_back(merge_bb);
		    builder->SetInsertPoint(merge_bb);

		    emit_phis(stmt->v.if_cond.exit, then_bb, else_bb, rhs_map, slice_flag);
		}
		break;

	    case STMT_WHILE_LOOP:
		{
		    BasicBlock *start_bb = builder->GetInsertBlock();
		    BasicBlock *entry_bb = BasicBlock::Create("entry", current_function);
		    BasicBlock *body_bb = BasicBlock::Create("body");
		    BasicBlock *exit_bb = BasicBlock::Create("exit");
		    map<rhs_t*, Value*> rhs_map;

		    emit_phi_rhss(stmt->v.while_loop.entry, true, &rhs_map, slice_flag);

		    builder->CreateBr(entry_bb);

		    builder->SetInsertPoint(entry_bb);

		    emit_phis(stmt->v.while_loop.entry, start_bb, NULL, rhs_map, slice_flag);

		    Value *invariant_number = emit_rhs(stmt->v.while_loop.invariant);
		    Value *invariant;

		    if (invariant_number->getType() == Type::Int32Ty)
			invariant = builder->CreateICmpNE(invariant_number, make_int_const(0));
		    else if (invariant_number->getType() == Type::FloatTy)
			invariant = builder->CreateFCmpONE(invariant_number, make_float_const(0.0));
		    else
			g_assert_not_reached();

		    builder->CreateCondBr(invariant, body_bb, exit_bb);

		    current_function->getBasicBlockList().push_back(body_bb);
		    builder->SetInsertPoint(body_bb);
		    emit_stmts(stmt->v.while_loop.body, slice_flag);
		    body_bb = builder->GetInsertBlock();
		    emit_phi_rhss(stmt->v.while_loop.entry, false, &rhs_map, slice_flag);
		    emit_phis(stmt->v.while_loop.entry, NULL, body_bb, rhs_map, slice_flag);
		    builder->CreateBr(entry_bb);

		    current_function->getBasicBlockList().push_back(exit_bb);
		    builder->SetInsertPoint(exit_bb);
		}
		break;

	    default:
		g_assert_not_reached();
		break;
	}
    }
}

void
code_emitter::build_const_value_info (value_t *value, statement_t *stmt, int const_type,
				      vector<const Type*> *struct_elems)
{
    if ((value->const_type | CONST_T) == (const_type | CONST_T)
	&& compiler_is_permanent_const_value(value))
    {
	if (!value->have_defined && value->index >= 0)
	{
	    compiler_print_value(value);
	    printf("   %d\n", next_const_value_index);

	    g_assert(const_value_index_map.find(value) == const_value_index_map.end());
	    const_value_index_map[value] = next_const_value_index++;

	    struct_elems->push_back(llvm_type_for_type(module, value->compvar->type));

	    value->have_defined = 1;
	}
    }
}

void
code_emitter::_build_const_value_info (value_t *value, statement_t *stmt, void *info)
{
    CLOSURE_VAR(code_emitter*, emitter, 0);
    CLOSURE_VAR(int, const_type, 1);
    CLOSURE_VAR(vector<const Type*>*, struct_elems, 2);

    emitter->build_const_value_info(value, stmt, const_type, struct_elems);
}

void
code_emitter::set_internals_from_invocation (Value *invocation_arg)
{
    set_internal(::lookup_internal(filter->v.mathmap.internals, "__canvasPixelW", true),
		 builder->CreateCall(module->getFunction(string("get_invocation_img_width")), invocation_arg));
    set_internal(::lookup_internal(filter->v.mathmap.internals, "__canvasPixelH", true),
		 builder->CreateCall(module->getFunction(string("get_invocation_img_height")), invocation_arg));
    set_internal(::lookup_internal(filter->v.mathmap.internals, "__renderPixelW", true),
		 builder->CreateCall(module->getFunction(string("get_invocation_render_width")), invocation_arg));
    set_internal(::lookup_internal(filter->v.mathmap.internals, "__renderPixelH", true),
		 builder->CreateCall(module->getFunction(string("get_invocation_render_height")), invocation_arg));
    set_internal(::lookup_internal(filter->v.mathmap.internals, "R", true),
		 builder->CreateCall(module->getFunction(string("get_invocation_image_R")), invocation_arg));
}

void
code_emitter::set_xy_vars_from_frame ()
{
    Value *xy_vars_untyped = builder->CreateCall(module->getFunction(string("get_frame_xy_vars")), frame_arg);
    xy_vars_var = builder->CreateBitCast(xy_vars_untyped, PointerType::getUnqual(xy_vars_type));
}

void
code_emitter::setup_xy_vars_from_closure ()
{
    Value *t_var = lookup_internal(::lookup_internal(filter->v.mathmap.internals, "t", true));
    Value *xy_vars_untyped = builder->CreateCall4(module->getFunction(string("calc_closure_xy_vars")),
						  invocation_arg, closure_arg, t_var, init_frame_function);
    xy_vars_var = builder->CreateBitCast(xy_vars_untyped, PointerType::getUnqual(xy_vars_type));
}

void
code_emitter::setup_filter_function (bool is_main_filter_function)
{
    Function *filter_function = is_main_filter_function
	? lookup_main_filter_function(module, filter)
	: lookup_filter_function(module, filter);
    Function::arg_iterator args = filter_function->arg_begin();
    Value *slice_arg = NULL;

    if (is_main_filter_function)
    {
	slice_arg = args++;
	slice_arg->setName("slice");
    }
    else
    {
	invocation_arg = args++;
	invocation_arg->setName("invocation");
    }
    closure_arg = args++;
    closure_arg->setName("closure");
    if (is_main_filter_function)
    {
	x_vars_var = args++;
	x_vars_var->setName("x_vars");
	y_vars_var = args++;
	y_vars_var->setName("y_vars");
    }
    set_internal(::lookup_internal(filter->v.mathmap.internals, "x", true), args++);
    set_internal(::lookup_internal(filter->v.mathmap.internals, "y", true), args++);
    set_internal(::lookup_internal(filter->v.mathmap.internals, "t", true), args++);
    pools_arg = args++;
    pools_arg->setName("pools");

    BasicBlock *block = BasicBlock::Create("entry", filter_function);

    builder = new IRBuilder<> (block);

    if (is_main_filter_function)
    {
	frame_arg = builder->CreateCall(module->getFunction(string("get_slice_frame")), slice_arg);
	invocation_arg = builder->CreateCall(module->getFunction(string("get_frame_invocation")), frame_arg);
    }

    set_internals_from_invocation(invocation_arg);

    if (is_main_filter_function)
	set_xy_vars_from_frame ();
    else
	setup_xy_vars_from_closure ();

    alloc_complex_copy_var();

    current_function = filter_function;
}

Value*
code_emitter::emit_sizeof (Type *type)
{
    Value *size = builder->CreateGEP(ConstantPointerNull::get(PointerType::getUnqual(type)),
				     make_int_const(1));

    return builder->CreatePtrToInt(size, sizeof(gpointer) == 4 ? Type::Int32Ty : Type::Int64Ty);
}

void
code_emitter::setup_init_frame_function ()
{
    Value *t_arg;
    Function::arg_iterator args = init_frame_function->arg_begin();

    invocation_arg = args++;
    invocation_arg->setName("invocation");
    //frame_arg = args++;
    //frame_arg->setName("frame");
    closure_arg = args++;
    closure_arg->setName("closure");
    t_arg = args++;
    t_arg->setName("t");
    pools_arg = args++;
    pools_arg->setName("pools");

    BasicBlock *block = BasicBlock::Create("entry", init_frame_function);

    builder = new IRBuilder<> (block);

    //invocation_arg = builder->CreateCall(module->getFunction(string("get_frame_invocation")), frame_arg);
    //pools_arg = builder->CreateCall(module->getFunction(string("get_frame_pools")), frame_arg);

    set_internal(::lookup_internal(filter->v.mathmap.internals, "t", true), t_arg);

    set_internals_from_invocation(invocation_arg);

    ret_var = builder->CreateCall2(module->getFunction(string("_pools_alloc")),
				   pools_arg, emit_sizeof(xy_vars_type));
    xy_vars_var = builder->CreateBitCast(ret_var, PointerType::getUnqual(xy_vars_type));

    alloc_complex_copy_var();

    current_function = init_frame_function;
}

Value*
code_emitter::setup_init_x_or_y_function (string function_name, const char *internal_name, StructType *vars_type)
{
    Constant *function_const = module->getOrInsertFunction(function_name,
							   PointerType::getUnqual(vars_type),	// ret type
							   get_slice_ptr_type(module), // slice
							   llvm_type_for_type(module, TYPE_IMAGE), // closure
							   Type::FloatTy, // x/y
							   Type::FloatTy, // t
							   NULL);
    current_function = cast<Function>(function_const);

    Value *slice_arg;

    Function::arg_iterator args = current_function->arg_begin();

    slice_arg = args++;
    slice_arg->setName("slice");
    closure_arg = args++;
    closure_arg->setName("closure");
    set_internal(::lookup_internal(filter->v.mathmap.internals, internal_name, true), args++);
    set_internal(::lookup_internal(filter->v.mathmap.internals, "t", true), args++);

    BasicBlock *block = BasicBlock::Create("entry", current_function);

    builder = new IRBuilder<> (block);

    frame_arg = builder->CreateCall(module->getFunction(string("get_slice_frame")), slice_arg);
    invocation_arg = builder->CreateCall(module->getFunction(string("get_frame_invocation")), frame_arg);
    pools_arg = builder->CreateCall(module->getFunction("get_slice_pools"), slice_arg);

    set_internals_from_invocation(invocation_arg);

    set_xy_vars_from_frame();

    Value *vars_untyped = builder->CreateCall2(module->getFunction(string("_pools_alloc")),
						 pools_arg, emit_sizeof(vars_type));
    Value *vars_var = builder->CreateBitCast(vars_untyped, PointerType::getUnqual(vars_type));

    alloc_complex_copy_var();

    return vars_var;
}

void
code_emitter::finish_function ()
{
    current_function = NULL;

    invocation_arg = NULL;
    closure_arg = NULL;
    frame_arg = NULL;
    pools_arg = NULL;

    x_vars_var = NULL;
    y_vars_var = NULL;
    xy_vars_var = NULL;
    ret_var = NULL;

    complex_copy_var = NULL;

    value_map.clear();
    internal_map.clear();

    if (builder)
    {
	delete builder;
	builder = NULL;
    }
}

StructType*
code_emitter::build_const_value_infos (int const_type)
{
    vector<const Type*> struct_elems;

    compiler_reset_have_defined(filter_code->first_stmt);
    next_const_value_index = 0;
    COMPILER_FOR_EACH_VALUE_IN_STATEMENTS(filter_code->first_stmt, &_build_const_value_info,
					  CLOSURE_ARG(this), CLOSURE_ARG((void*)const_type), CLOSURE_ARG(&struct_elems));

    return StructType::get(struct_elems);
}

void
code_emitter::emit_init_frame_function ()
{
#ifdef DEBUG_OUTPUT
    printf("emitting init frame for %s\n", filter->name);
#endif

    xy_vars_type = build_const_value_infos(CONST_X | CONST_Y);

    setup_init_frame_function();
    compiler_slice_code_for_const(filter_code->first_stmt, CONST_X | CONST_Y);
    emit_stmts(filter_code->first_stmt, SLICE_XY_CONST);
    builder->CreateRet(ret_var);
    finish_function();
}

void
code_emitter::emit_filter_function ()
{
#ifdef DEBUG_OUTPUT
    printf("emitting filter func for %s\n", filter->name);
#endif

    setup_filter_function(false);

    x_vars_type = build_const_value_infos(CONST_X);
    x_vars_var = builder->CreateAlloca(x_vars_type);
    compiler_slice_code_for_const(filter_code->first_stmt, CONST_X);
    emit_stmts(filter_code->first_stmt, SLICE_X_CONST);
    value_map.clear();

    y_vars_type = build_const_value_infos(CONST_Y);
    y_vars_var = builder->CreateAlloca(y_vars_type);
    compiler_slice_code_for_const(filter_code->first_stmt, CONST_Y);
    emit_stmts(filter_code->first_stmt, SLICE_Y_CONST);
    value_map.clear();

    compiler_slice_code_for_const(filter_code->first_stmt, CONST_NONE);
    emit_stmts(filter_code->first_stmt, SLICE_NO_CONST);

    finish_function();
}

void
code_emitter::emit_main_filter_funcs ()
{
    // init x
#ifdef DEBUG_OUTPUT
    printf("emitting init x for %s\n", filter->name);
#endif
    x_vars_var = setup_init_x_or_y_function(init_x_function_name(filter), "y", x_vars_type);
    compiler_slice_code_for_const(filter_code->first_stmt, CONST_X);
    emit_stmts(filter_code->first_stmt, SLICE_X_CONST);
    builder->CreateRet(x_vars_var);
    finish_function();

    // init y
#ifdef DEBUG_OUTPUT
    printf("emitting init y for %s\n", filter->name);
#endif
    y_vars_var = setup_init_x_or_y_function(init_y_function_name(filter), "x", y_vars_type);
    compiler_slice_code_for_const(filter_code->first_stmt, CONST_Y);
    emit_stmts(filter_code->first_stmt, SLICE_Y_CONST);
    builder->CreateRet(y_vars_var);
    finish_function();

    // filter
#ifdef DEBUG_OUTPUT
    printf("emitting main filter func for %s\n", filter->name);
#endif
    module->getOrInsertFunction(main_filter_function_name(filter),
				llvm_type_for_type(module, TYPE_TUPLE), // ret type
				get_slice_ptr_type(module), // invocation
				llvm_type_for_type(module, TYPE_IMAGE), // closure
				PointerType::getUnqual(x_vars_type), // x_vars
				PointerType::getUnqual(y_vars_type), // y_vars
				Type::FloatTy, // x
				Type::FloatTy, // y
				Type::FloatTy, // t
				get_pools_ptr_type(module), // pools
				NULL);
    setup_filter_function(true);
    compiler_slice_code_for_const(filter_code->first_stmt, CONST_NONE);
    emit_stmts(filter_code->first_stmt, SLICE_NO_CONST);
    finish_function();
}

static Function*
make_filter_function (Module *module, filter_t *filter)
{
    Constant *function_const = module->getOrInsertFunction(filter_function_name(filter),
							   llvm_type_for_type(module, TYPE_TUPLE), // ret type
							   get_invocation_ptr_type(module), // invocation
							   llvm_type_for_type(module, TYPE_IMAGE), // closure
							   Type::FloatTy, // x
							   Type::FloatTy, // y
							   Type::FloatTy, // t
							   get_pools_ptr_type(module), // pools
							   NULL);
    return cast<Function>(function_const);
}

static Function*
make_init_frame_function (Module *module, filter_t *filter)
{
    Constant *function_const = module->getOrInsertFunction(init_frame_function_name(filter),
							   get_void_ptr_type(), // ret type
							   get_invocation_ptr_type(module), // invocation
							   llvm_type_for_type(module, TYPE_IMAGE), // closure
							   Type::FloatTy, // t
							   get_pools_ptr_type(module), // pools
							   NULL);
    return cast<Function>(function_const);
}

void* lazy_creator (const std::string &name);

extern "C"
void
gen_and_load_llvm_code (mathmap_t *mathmap, char *template_filename)
{
    filter_code_t **filter_codes = compiler_compile_filters(mathmap);
    MemoryBuffer *buffer = MemoryBuffer::getFile(template_filename, NULL);

    g_assert(buffer != NULL);

    Module *module = ParseBitcodeFile (buffer, NULL);
    int i;
    filter_t *filter;

    assert(module != NULL);
    delete buffer;

    for (i = 0, filter = mathmap->filters;
	 filter != 0;
	 ++i, filter = filter->next)
    {
	if (filter->kind != FILTER_MATHMAP)
	    continue;

	make_init_frame_function(module, filter);
	make_filter_function(module, filter);
    }

    for (i = 0, filter = mathmap->filters;
	 filter != 0;
	 ++i, filter = filter->next)
    {
	filter_code_t *code = filter_codes[i];
	code_emitter *emitter;

	if (filter->kind != FILTER_MATHMAP)
	    continue;

	g_assert(code->filter == filter);

	emitter = new code_emitter (module, filter, code);
	try
	{
	    emitter->emit_init_frame_function();
	    emitter->emit_filter_function();
	    if (filter == mathmap->main_filter)
		emitter->emit_main_filter_funcs();
	}
	catch (compiler_error error)
	{
	    delete emitter;
	    delete module;

	    strcpy(error_string, error.info.c_str());
	    return;
	}
	delete emitter;
    }

    compiler_free_pools(mathmap);

#ifdef DEBUG_OUTPUT
    module->dump();

    verifyModule(*module, PrintMessageAction);
#endif

    PassManager pm;
    pm.add (new TargetData (module));
    pm.add (createFunctionInliningPass ());
    pm.run(*module);

    ExecutionEngine *ee = ExecutionEngine::create (module);

    ee->InstallLazyFunctionCreator(lazy_creator);

    void *init_frame_fptr = ee->getPointerToFunction(lookup_init_frame_function(module, mathmap->main_filter));
    void *filter_fptr = ee->getPointerToFunction(lookup_filter_function(module, mathmap->main_filter));
    void *main_filter_fptr = ee->getPointerToFunction(lookup_main_filter_function(module, mathmap->main_filter));
    void *init_x_fptr = ee->getPointerToFunction(lookup_init_x_function(module, mathmap->main_filter));
    void *init_y_fptr = ee->getPointerToFunction(lookup_init_y_function(module, mathmap->main_filter));
    g_assert(main_filter_fptr && init_x_fptr && init_y_fptr);

    mathmap->llvm_init_frame_func = (llvm_init_frame_func_t)init_frame_fptr;
    mathmap->filter_func = (filter_func_t)filter_fptr;
    mathmap->main_filter_func = (llvm_filter_func_t)main_filter_fptr;
    mathmap->init_x_func = (init_x_or_y_func_t)init_x_fptr;
    mathmap->init_y_func = (init_x_or_y_func_t)init_y_fptr;

    mathmap->module_info = ee;
}

extern "C"
void
unload_llvm_code (void *module_info)
{
    ExecutionEngine *ee = (ExecutionEngine*)module_info;

    delete ee;
}
