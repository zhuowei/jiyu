
#ifndef LLVM_H
#define LLVM_H

#include "general.h"

namespace llvm {
    class Module;
    class LLVMContext;
    
    class Value;
    
    class Type;
    class FunctionType;
    class StructType;
    class Function;
    
    class TargetMachine;
    class ConstantFolder;
    class IRBuilderDefaultInserter;
    
    template<typename T, typename Inserter> class IRBuilder;
    
    namespace orc {
        class ExecutionSession;
        class RTDyldObjectLinkingLayer;
        class IRCompileLayer;
        class ThreadSafeContext;
        class MangleAndInterner;
    };
};

struct Compiler;
struct Ast_Function;
struct Ast_Type_Info;
struct Ast_Declaration;
struct Ast_Scope;
struct Ast_Expression;
struct Ast_Literal;

struct LLVM_Generator {
    Compiler *compiler;
    
    llvm::TargetMachine *TargetMachine;
    
    String obj_output_name;
    llvm::Module  *llvm_module;
    llvm::LLVMContext *llvm_context;
    llvm::orc::ThreadSafeContext *thread_safe_context;
    llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter> *irb;
    
    llvm::Type *type_void;
    llvm::Type *type_i1;
    llvm::Type *type_i8;
    llvm::Type *type_i16;
    llvm::Type *type_i32;
    llvm::Type *type_i64;
    llvm::Type *type_f32;
    llvm::Type *type_f64;
    
    llvm::StructType *type_string;
    llvm::Type *type_string_length;
    
    llvm::Type *type_intptr;
    
    Array<Tuple<Ast_Declaration *, llvm::Value *>> decl_value_map;
    
    
    LLVM_Generator(Compiler *compiler) {
        this->compiler = compiler;
    }
    
    void init();
    void finalize();
    
    llvm::Value *create_string_literal(Ast_Literal *lit);
    llvm::Value *get_value_for_decl(Ast_Declaration *decl);
    llvm::Value *dereference(llvm::Value *value, s64 element_path_index, bool is_lvalue = false);
    
    llvm::Function *get_or_create_function(Ast_Function *function);
    llvm::Type *get_type(Ast_Type_Info *type);
    llvm::FunctionType *create_function_type(Ast_Function *function);
    void emit_scope(Ast_Scope *scope);
    void emit_function(Ast_Function *function);
    llvm::Value *emit_expression(Ast_Expression *expression, bool is_lvalue = false);
};

struct LLVM_Jitter {
    /*
    llvm::orc::ExecutionSession         *execution_session;
    llvm::orc::RTDyldObjectLinkingLayer *object_layer;
    llvm::orc::IRCompileLayer           *compile_layer;
    llvm::orc::MangleAndInterner        *mangler;
    */
    
    Compiler *compiler;
    LLVM_Generator *llvm;
    
    LLVM_Jitter(LLVM_Generator *_llvm) {
        this->llvm     = _llvm;
        this->compiler = _llvm->compiler;
    }
    
    void init();
    void *lookup_symbol(String name);
};


#endif
