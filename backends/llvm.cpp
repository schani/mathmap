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

    void emit ();

private:
    filter_t *filter;
    filter_code_t *filter_code;

    Module *module;
    IRBuilder<> *builder;
    Function *filter_function;
    map<value_t*, Value*> value_map;
    map<string, Value*> internal_map;

    Value *invocation_arg;
    Value *closure_arg;
    Value *pools_arg;

    void set_value (value_t *value, Value *llvm_value);
    Value* lookup_value (value_t *value);

    void set_internal (internal_t *internal, Value *llvm_value);
    Value* lookup_internal (internal_t *internal);
    Value* lookup_internal (const char *name);

    const Type* llvm_type_for_type (type_t type);

    Value* promote (Value *val, int type);

    void emit_stmts (statement_t *stmt, unsigned int slice_flag);
    void emit_phi_rhss (statement_t *stmt, bool left, map<rhs_t*, Value*> *rhs_map);
    void emit_phis (statement_t *stmt, BasicBlock *left_bb, BasicBlock *right_bb, map<rhs_t*, Value*> &rhs_map);
    Value* emit_rhs (rhs_t *rhs);
    Value* emit_primary (primary_t *primary, bool need_float = false);
    Value* emit_closure (filter_t *filter, primary_t *args);
};

string
filter_function_name (filter_t *filter)
{
    return string("filter_") + string(filter->name);
}

Function*
lookup_filter_function (Module *module, filter_t *filter)
{
    Function *func = module->getFunction(filter_function_name(filter));

    g_assert(func != NULL);

    return func;
}

code_emitter::code_emitter (Module *_module, filter_t *_filter, filter_code_t *code)
{
    module = _module;
    filter_code = code;
    filter = _filter;

    filter_function = lookup_filter_function(module, filter);
    g_assert(filter_function != NULL);

    Function::arg_iterator args = filter_function->arg_begin();

    invocation_arg = args++;
    invocation_arg->setName("invocation");
    closure_arg = args++;
    closure_arg->setName("closure");
    set_internal(::lookup_internal(filter->v.mathmap.internals, "x", true), args++);
    set_internal(::lookup_internal(filter->v.mathmap.internals, "y", true), args++);
    set_internal(::lookup_internal(filter->v.mathmap.internals, "t", true), args++);
    pools_arg = args++;
    pools_arg->setName("pools");

    BasicBlock *block = BasicBlock::Create("entry", filter_function);

    builder = new IRBuilder<> (block);

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

code_emitter::~code_emitter ()
{
    delete builder;
}

void
code_emitter::set_value (value_t *value, Value *llvm_value)
{
    g_assert(value);
    g_assert(llvm_value);
    g_assert(value_map.find(value) == value_map.end());
    value_map[value] = llvm_value;
}

Value*
code_emitter::lookup_value (value_t *value)
{
    g_assert(value_map.find(value) != value_map.end());
    return value_map[value];
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
		case TYPE_V2 :
		    assert(!need_float);
		    return builder->CreateCall2(module->getFunction(string("make_v2")),
						make_float_const(primary->v.constant.v2_value.v[0]),
						make_float_const(primary->v.constant.v2_value.v[1]));
		case TYPE_V3 :
		    assert(!need_float);
		    return builder->CreateCall3(module->getFunction(string("make_v3")),
						make_float_const(primary->v.constant.v2_value.v[0]),
						make_float_const(primary->v.constant.v2_value.v[1]),
						make_float_const(primary->v.constant.v2_value.v[2]));
		case TYPE_M2X2 :
		    assert(!need_float);
		    return builder->CreateCall4(module->getFunction(string("make_m2x2")),
						make_float_const(primary->v.constant.m2x2_value.a00),
						make_float_const(primary->v.constant.m2x2_value.a01),
						make_float_const(primary->v.constant.m2x2_value.a10),
						make_float_const(primary->v.constant.m2x2_value.a11));
		default :
		    g_assert_not_reached();
	    }

	default:
	    g_assert_not_reached();
    }
}

static const char*
get_closure_set_arg_func_name (int type)
{
    switch (type)
    {
	case USERVAL_INT_CONST :
	    return "set_closure_arg_int";
	case USERVAL_FLOAT_CONST :
	    return "set_closure_arg_float";
	case USERVAL_BOOL_CONST :
	    return "set_closure_arg_bool";
	case USERVAL_COLOR :
	    return "set_closure_arg_color";
	case USERVAL_CURVE :
	    return "set_closure_arg_curve";
	case USERVAL_GRADIENT :
	    return "set_closure_arg_gradient";
	case USERVAL_IMAGE :
	    return "set_closure_arg_image";
	default :
	    g_assert_not_reached();
    }
}

Value*
code_emitter::emit_closure (filter_t *closure_filter, primary_t *args)
{
    int num_args = compiler_num_filter_args(closure_filter) - 3;
    Value *closure;
    bool have_size;
    userval_info_t *info;
    int i;

    g_assert(closure_filter->kind == FILTER_MATHMAP);

    closure = builder->CreateCall4(module->getFunction(string("alloc_closure_image")),
				   invocation_arg, pools_arg, make_int_const(num_args),
				   lookup_filter_function(module, closure_filter));

    have_size = FALSE;
    for (i = 0, info = closure_filter->userval_infos;
	 info != 0;
	 ++i, info = info->next)
    {
	const char *set_func_name = get_closure_set_arg_func_name(info->type);

	if (info->type == USERVAL_IMAGE)
	{
	    builder->CreateCall4(module->getFunction(string(set_func_name)),
				 closure, make_int_const(i), emit_primary(&args[i]), make_int_const(!have_size));
	    have_size = true;
	}
	else
	    builder->CreateCall3(module->getFunction(string(set_func_name)),
				 closure, make_int_const(i), emit_primary(&args[i]));
    }
    g_assert(i == num_args);

    if (!have_size)
	builder->CreateCall3(module->getFunction(string("set_closure_pixel_size")),
			     closure, lookup_internal("__canvasPixelW"), lookup_internal("__canvasPixelH"));

    return closure;
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
		    val->dump();
		    args.push_back(val);
		}
		func->dump();
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
			Value *local = builder->CreateAlloca(llvm_type_for_type(TYPE_COMPLEX));
			Value *local_ptr = builder->CreateBitCast(local, PointerType::getUnqual(Type::Int64Ty));
			builder->CreateStore(result, local_ptr);
			result = local;
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
	    {
		if (rhs->v.closure.filter->kind == FILTER_MATHMAP)
		    return emit_closure(rhs->v.closure.filter, rhs->v.closure.args);
		else
		{
		    throw compiler_error(string("Native filters don't work with the LLVM backend, yet."));
		}
	    }

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
		    std::cout << "setting tuple elem to type ";
		    val->getType()->print(std::cout);
		    std::cout << endl;
		    builder->CreateCall3(set_func, tuple, make_int_const(i), val);
		}
		return tuple;
	    }

	default :
	    g_assert_not_reached();
    }
}

const Type*
code_emitter::llvm_type_for_type (type_t type)
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
	default :
	    g_assert_not_reached();
    }
}

void
code_emitter::emit_phi_rhss (statement_t *stmt, bool left, map<rhs_t*, Value*> *rhs_map)
{
    while (stmt != NULL)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
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

	stmt = stmt->next;
    }
}

void
code_emitter::emit_phis (statement_t *stmt, BasicBlock *left_bb, BasicBlock *right_bb, map<rhs_t*, Value*> &rhs_map)
{
    while (stmt != NULL)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		{
		    Value *left = NULL;
		    Value *right = NULL;
		    int compvar_type = stmt->v.assign.lhs->compvar->type;
		    const Type *type = llvm_type_for_type(compvar_type);

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
			set_value(stmt->v.assign.lhs, phi);
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

	stmt = stmt->next;
    }
}

void
code_emitter::emit_stmts (statement_t *stmt, unsigned int slice_flag)
{
    while (stmt != NULL)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
		compiler_print_assign_statement(stmt);
		printf("\n");
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

		    BasicBlock *then_bb = BasicBlock::Create("then", filter_function);
		    BasicBlock *else_bb = BasicBlock::Create("else");
		    BasicBlock *merge_bb = BasicBlock::Create("ifcont");

		    builder->CreateCondBr(condition, then_bb, else_bb);

		    builder->SetInsertPoint(then_bb);
		    emit_stmts(stmt->v.if_cond.consequent, slice_flag);
		    emit_phi_rhss(stmt->v.if_cond.exit, true, &rhs_map);
		    builder->CreateBr(merge_bb);
		    then_bb = builder->GetInsertBlock();

		    filter_function->getBasicBlockList().push_back(else_bb);
		    builder->SetInsertPoint(else_bb);
		    emit_stmts(stmt->v.if_cond.alternative, slice_flag);
		    emit_phi_rhss(stmt->v.if_cond.exit, false, &rhs_map);
		    builder->CreateBr(merge_bb);
		    else_bb = builder->GetInsertBlock();

		    filter_function->getBasicBlockList().push_back(merge_bb);
		    builder->SetInsertPoint(merge_bb);

		    emit_phis(stmt->v.if_cond.exit, then_bb, else_bb, rhs_map);
		}
		break;

	    case STMT_WHILE_LOOP:
		{
		    BasicBlock *start_bb = builder->GetInsertBlock();
		    BasicBlock *entry_bb = BasicBlock::Create("entry", filter_function);
		    BasicBlock *body_bb = BasicBlock::Create("body");
		    BasicBlock *exit_bb = BasicBlock::Create("exit");
		    map<rhs_t*, Value*> rhs_map;

		    emit_phi_rhss(stmt->v.while_loop.entry, true, &rhs_map);

		    builder->CreateBr(entry_bb);

		    builder->SetInsertPoint(entry_bb);

		    emit_phis(stmt->v.while_loop.entry, start_bb, NULL, rhs_map);

		    Value *invariant_number = emit_rhs(stmt->v.while_loop.invariant);
		    Value *invariant;

		    if (invariant_number->getType() == Type::Int32Ty)
			invariant = builder->CreateICmpNE(invariant_number, make_int_const(0));
		    else if (invariant_number->getType() == Type::FloatTy)
			invariant = builder->CreateFCmpONE(invariant_number, make_float_const(0.0));
		    else
			g_assert_not_reached();

		    builder->CreateCondBr(invariant, body_bb, exit_bb);

		    filter_function->getBasicBlockList().push_back(body_bb);
		    builder->SetInsertPoint(body_bb);
		    emit_stmts(stmt->v.while_loop.body, slice_flag);
		    body_bb = builder->GetInsertBlock();
		    emit_phi_rhss(stmt->v.while_loop.entry, false, &rhs_map);
		    emit_phis(stmt->v.while_loop.entry, NULL, body_bb, rhs_map);
		    builder->CreateBr(entry_bb);

		    filter_function->getBasicBlockList().push_back(exit_bb);
		    builder->SetInsertPoint(exit_bb);
		}
		break;

	    default:
		g_assert_not_reached();
		break;
	}

	stmt = stmt->next;
    }
}

void
code_emitter::emit ()
{
    emit_stmts(filter_code->first_stmt, SLICE_IGNORE);
}

static Function*
make_filter_function (Module *module, filter_t *filter)
{
    const Type *invocation_type = module->getTypeByName(string("struct.mathmap_invocation_t"));
    const Type *image_type = module->getTypeByName(string("struct._image_t"));
    const Type *pools_type = module->getTypeByName(string("struct.pools_t"));
    assert(invocation_type && image_type && pools_type);
    Constant *function_const = module->getOrInsertFunction(filter_function_name(filter),
							   PointerType::getUnqual(Type::FloatTy), // ret type
							   PointerType::getUnqual(invocation_type), // invocation
							   PointerType::getUnqual(image_type), // closure
							   Type::FloatTy, // x
							   Type::FloatTy, // y
							   Type::FloatTy, // t
							   PointerType::getUnqual(pools_type), // pools
							   NULL);
    return cast<Function>(function_const);
}

static void*
lazy_creator (const std::string &name)
{
    std::cout << "resolving func " << name << endl;
    if (name == "get_orig_val_pixel")
	return (void*)get_orig_val_pixel;
    g_assert_not_reached ();
}

extern "C"
filter_func_t
gen_and_load_llvm_code (mathmap_t *mathmap, char *template_filename)
{
    filter_code_t **filter_codes = compiler_compile_filters(mathmap);
    MemoryBuffer *buffer = MemoryBuffer::getFile(template_filename, NULL);
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
	    emitter->emit();
	}
	catch (compiler_error error)
	{
	    delete emitter;
	    delete module;

	    strcpy(error_string, error.info.c_str());
	    return NULL;
	}
	delete emitter;
    }

    verifyModule(*module, PrintMessageAction);

    PassManager pm;
    pm.add (new TargetData (module));
    pm.add (createFunctionInliningPass ());
    pm.run(*module);

    module->dump();

    ExecutionEngine *ee = ExecutionEngine::create (module);

    ee->InstallLazyFunctionCreator(lazy_creator);

    Function *main_filter_function = lookup_filter_function(module, mathmap->main_filter);
    void *fptr = ee->getPointerToFunction (main_filter_function);
    assert(fptr);

    compiler_free_pools(mathmap);

    mathmap->module_info = ee;

    return (filter_func_t)fptr;
}

extern "C"
void
unload_llvm_code (void *module_info)
{
    ExecutionEngine *ee = (ExecutionEngine*)module_info;

    std::cout << "deleting execution engine" << endl;

    delete ee;
}
