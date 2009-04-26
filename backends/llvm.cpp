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

#include <assert.h>

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

class code_emitter
{
public:
    code_emitter (Module *module, filter_code_t *code);
    ~code_emitter ();

    void emit ();

    Function* get_filter_function ();

private:
    Module *module;
    IRBuilder<> *builder;
    Function *filter_function;
    filter_code_t *filter_code;
    map<value_t*, Value*> value_map;
    map<internal_t*, Value*> internal_map;

    Value *invocation_arg;
    Value *closure_arg;
    Value *x_arg;
    Value *y_arg;
    Value *t_arg;
    Value *pools_arg;

    void set_value (value_t *value, Value *llvm_value);
    Value* lookup_value (value_t *value);

    Value* lookup_internal (internal_t *internal);

    void emit_stmts (statement_t *stmt, unsigned int slice_flag);
    Value* emit_rhs (rhs_t *rhs);
    Value* emit_primary (primary_t *primary, bool need_float = false);
};

code_emitter::code_emitter (Module *_module, filter_code_t *code)
{
    module = _module;
    filter_code = code;

    /*
    TypeSymbolTable &tst = module->getTypeSymbolTable ();

    for (TypeSymbolTable::iterator iter = tst.begin(); iter != tst.end(); ++iter) {
	std::cout << "type " << iter->first << endl;
    }
    */

    const Type *invocation_type = module->getTypeByName(string("struct.mathmap_invocation_t"));
    const Type *image_type = module->getTypeByName(string("struct._image_t"));
    const Type *pools_type = module->getTypeByName(string("struct.pools_t"));
    assert(invocation_type && image_type && pools_type);
    Constant *function_const = module->getOrInsertFunction("filter_func",
							   PointerType::getUnqual(Type::FloatTy), // ret type
							   PointerType::getUnqual(invocation_type), // invocation
							   PointerType::getUnqual(image_type), // closure
							   Type::FloatTy, // x
							   Type::FloatTy, // y
							   Type::FloatTy, // t
							   PointerType::getUnqual(pools_type), // pools
							   NULL);
    filter_function = cast<Function>(function_const);
    Function::arg_iterator args = filter_function->arg_begin();

    invocation_arg = args++;
    invocation_arg->setName("invocation");
    closure_arg = args++;
    closure_arg->setName("closure");
    x_arg = args++;
    x_arg->setName("x");
    y_arg = args++;
    y_arg->setName("y");
    t_arg = args++;
    t_arg->setName("t");
    pools_arg = args++;
    pools_arg->setName("pools");

    BasicBlock *block = BasicBlock::Create("entry", filter_function);

    builder = new IRBuilder<> (block);
}

code_emitter::~code_emitter ()
{
    delete builder;
}

Function*
code_emitter::get_filter_function ()
{
    return filter_function;
}

void
code_emitter::set_value (value_t *value, Value *llvm_value)
{
    g_assert(value_map.find(value) == value_map.end());
    value_map[value] = llvm_value;
}

Value*
code_emitter::lookup_value (value_t *value)
{
    g_assert(value_map.find(value) != value_map.end());
    return value_map[value];
}

Value*
code_emitter::lookup_internal (internal_t *internal)
{
    g_assert(internal_map.find(internal) != internal_map.end());
    return internal_map[internal];
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
code_emitter::emit_primary (primary_t *primary, bool need_float)
{
    switch (primary->kind)
    {
	case PRIMARY_VALUE :
	    {
		Value *val = lookup_value(primary->v.value);

		if (need_float)
		    assert(val->getType() == Type::FloatTy);
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
		Function *func = module->getFunction(string(compiler_function_name_for_op_rhs(rhs)));
		g_assert(func);
		std::vector<Value*> args;
		args.push_back(invocation_arg);
		args.push_back(pools_arg);
		for (int i = 0; i < rhs->v.op.op->num_args; ++i)
		    args.push_back(emit_primary(&rhs->v.op.args[i]));
		return builder->CreateCall(func, args.begin(), args.end());
	    }

	case RHS_FILTER :
	case RHS_CLOSURE :
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
	    g_assert_not_reached ();
    }
}

void
code_emitter::emit_stmts (statement_t *stmt, unsigned int slice_flag)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
		if (stmt->v.assign.rhs->kind == RHS_OP
		    && stmt->v.assign.rhs->v.op.op->index == OP_OUTPUT_TUPLE)
		    builder->CreateRet(emit_primary(&stmt->v.assign.rhs->v.op.args[0]));
		else
		    set_value(stmt->v.assign.lhs, emit_rhs(stmt->v.assign.rhs));
		break;

	    case STMT_PHI_ASSIGN :
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

extern "C"
filter_func_t
gen_and_load_llvm_code (mathmap_t *mathmap, void **module_info, char *template_filename)
{
    filter_code_t **filter_codes = compiler_compile_filters(mathmap);
    MemoryBuffer *buffer = MemoryBuffer::getFile(template_filename, NULL);
    Module *module = ParseBitcodeFile (buffer, NULL);
    code_emitter *emitter;
    Function *filter_function;

    assert(module != NULL);
    delete buffer;

    emitter = new code_emitter (module, filter_codes[0]);
    emitter->emit();
    filter_function = emitter->get_filter_function();
    delete emitter;

    verifyModule(*module, PrintMessageAction);

    PassManager pm;
    pm.add (new TargetData (module));
    pm.add (createFunctionInliningPass ());
    pm.run(*module);

    module->dump();

    ExecutionEngine *ee = ExecutionEngine::create (module);

    void *fptr = ee->getPointerToFunction (filter_function);
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
