
#ifndef LLVM_H
#define LLVM_H

#include "general.h"

namespace llvm {
    class Module;
    class LLVMContext;

    class Type;
    class FunctionType;

};

#include "llvm/IR/IRBuilder.h"

struct Compiler;
struct Ast_Function;
struct Ast_Type_Info;
struct Ast_Declaration;
struct Ast_Scope;
struct Ast_Expression;

struct LLVM_Generator {
    Compiler *compiler;

    String obj_output_name;
    llvm::Module  *llvm_module;
    llvm::LLVMContext *llvm_context;
    llvm::IRBuilder<> *irb;

    llvm::Type *type_void;
    llvm::Type *type_i8;
    llvm::Type *type_i16;
    llvm::Type *type_i32;
    llvm::Type *type_i64;

    Array<Tuple<Ast_Declaration *, llvm::Value *>> decl_value_map;


    LLVM_Generator(Compiler *compiler) {
        this->compiler = compiler;
    }

    void init();

    llvm::Value *get_value_for_decl(Ast_Declaration *decl);

    llvm::Type *get_type(Ast_Type_Info *type);
    llvm::FunctionType *create_function_type(Ast_Function *function);
    void emit_scope(Ast_Scope *scope);
    void emit_function(Ast_Function *function);
    llvm::Value *emit_expression(Ast_Expression *expression);
};


#endif
