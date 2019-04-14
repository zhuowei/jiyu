

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

Value *LLVM_Generator::get_value_for_decl(Ast_Declaration *decl) {
    for (auto &it : decl_value_map) {
        if (it.item1 == decl) {
            return it.item2;
        }
    }

    return nullptr;
}

static Value *create_alloca_in_entry(IRBuilder<> *irb, Type *type) {
    auto current_block = irb->GetInsertBlock();

    auto func = current_block->getParent();

    BasicBlock *entry = &func->getEntryBlock();
    irb->SetInsertPoint(entry->getTerminator());
    Value *alloca = irb->CreateAlloca(type);

    irb->SetInsertPoint(current_block);

    return alloca;
}

Value *LLVM_Generator::emit_expression(Ast_Expression *expression) {
    switch (expression->type) {
        case AST_SCOPE: {
            auto scope = static_cast<Ast_Scope *>(expression);
            emit_scope(scope);

            return nullptr;
        }

        case AST_BINARY_EXPRESSION: {
            auto bin = static_cast<Ast_Binary_Expression *>(expression);

            Value *left  = emit_expression(bin->left);
            Value *right = emit_expression(bin->right);

            if (bin->operator_type == Token::EQUALS) {
                // if we got here, left must be a pointer type

                irb->CreateStore(right, left);
                return nullptr;
            } else {
                assert(false);
            }
        }

        case AST_LITERAL: {
            auto lit = static_cast<Ast_Literal *>(expression);

            switch (lit->literal_type) {
                case Ast_Literal::INTEGER: return ConstantInt::get(get_type(lit->type_info), lit->integer_value);
                default: return nullptr;
            }
        }

        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            assert(ident->resolved_declaration);

            assert(ident->resolved_declaration->type == AST_DECLARATION);
            auto decl = static_cast<Ast_Declaration *>(ident->resolved_declaration);

            return get_value_for_decl(decl);
        }

        case AST_DECLARATION: {
            auto decl = static_cast<Ast_Declaration *>(expression);
            if (decl->initializer_expression) {
                auto value = emit_expression(decl->initializer_expression);
                irb->CreateStore(value, get_value_for_decl(decl));
            }
            return nullptr;
        }
    }

    return nullptr;
}

void LLVM_Generator::emit_scope(Ast_Scope *scope) {
    // setup variable mappings
    for (auto &it : scope->declarations) {
        if (it->type != AST_DECLARATION) continue;
        auto decl = static_cast<Ast_Declaration *>(it);

        auto alloca = create_alloca_in_entry(irb, get_type(it->type_info));

        assert(get_value_for_decl(decl) == nullptr);
        decl_value_map.add(MakeTuple(decl, alloca));
    }

    for (auto &it : scope->statements) {
        emit_expression(it);
    }
}

void LLVM_Generator::emit_function(Ast_Function *function) {
    assert(function->identifier && function->identifier->name);

    FunctionType *function_type = create_function_type(function);
    Function *func = Function::Create(function_type, GlobalValue::LinkageTypes::ExternalLinkage, string_ref(function->identifier->name->name), llvm_module);

    // create entry block
    BasicBlock *entry = BasicBlock::Create(*llvm_context, "entry", func);
    BasicBlock *starting_block = BasicBlock::Create(*llvm_context, "start", func);

    irb->SetInsertPoint(entry);
    irb->CreateBr(starting_block);

    irb->SetInsertPoint(starting_block);
    emit_scope(function->scope);

    func->dump();
    decl_value_map.clear();
}

