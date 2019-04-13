

#include "llvm.h"
#include "ast.h"
#include "compiler.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

using namespace llvm;

void LLVM_Generator::init() {
    llvm_context = new LLVMContext();
    llvm_module = new Module("Htn Module", *llvm_context);
    irb = new IRBuilder<>(*llvm_context);

    type_void = Type::getVoidTy(*llvm_context);
    type_i8   = Type::getInt8Ty(*llvm_context);
    type_i16  = Type::getInt16Ty(*llvm_context);
    type_i32  = Type::getInt32Ty(*llvm_context);
    type_i64  = Type::getInt64Ty(*llvm_context);
}

static StringRef string_ref(String s) {
    return StringRef(s.data, s.length);
}

Type *LLVM_Generator::get_type(Ast_Type_Info *type) {
    if (type->type == Ast_Type_Info::VOID) {
        return type_void;
    }

    if (type->type == Ast_Type_Info::INTEGER) {
        switch (type->size) {
            case 1: return type_i8;
            case 2: return type_i16;
            case 4: return type_i32;
            case 8: return type_i64;
            default: assert(false);
        }
    }

    assert(false);
    return nullptr;
}

FunctionType *LLVM_Generator::create_function_type(Ast_Function *function) {
    Array<Type *> arguments;

    for (auto &arg : function->arguments) {
        if (arg->type_info == compiler->type_void) continue;

        Type *type = get_type(arg->type_info);
        arguments.add(type);
    }

    for (auto &ret : function->returns) {
        if (ret->type_info == compiler->type_void) continue;
        Type *type = get_type(ret->type_info)->getPointerTo();
        arguments.add(type);
    }
    return FunctionType::get(type_void, ArrayRef<Type *>(arguments.data, arguments.count), false);
}

void LLVM_Generator::emit_function(Ast_Function *function) {
    assert(function->identifier && function->identifier->name);

    FunctionType *function_type = create_function_type(function);
    Function *func = Function::Create(function_type, GlobalValue::LinkageTypes::ExternalLinkage, string_ref(function->identifier->name->name), llvm_module);

    // create entry block
    BasicBlock *entry = BasicBlock::Create(*llvm_context, "entry", func);

    func->dump();
}

