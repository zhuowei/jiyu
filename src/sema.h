
#ifndef SEMA_H
#define SEMA_H

#include "general.h"

struct Compiler;
struct Ast_Scope;
struct Ast_Expression;
struct Ast_Function;
struct Ast_Type_Info;
struct Atom;
struct Ast_Type_Instantiation;

struct Sema {
    Compiler *compiler;
    
    Sema(Compiler *compiler) {
        this->compiler = compiler;
    }
    
    Ast_Expression *find_declaration_for_atom(Atom *atom, Ast_Scope *start);
    Ast_Expression *find_declaration_for_atom_in_scope(Ast_Scope *scope, Atom *atom);
    
    Ast_Type_Info *resolve_type_inst(Ast_Type_Instantiation *type_inst);
    
    void typecheck_scope(Ast_Scope *scope);
    void typecheck_and_implicit_cast_expression_pair(Ast_Expression *left, Ast_Expression *right, Ast_Expression **result_left, Ast_Expression **result_right, bool allow_coerce_to_ptr_void);
    void typecheck_expression(Ast_Expression *expression, Ast_Type_Info *want_numeric_type = nullptr);
    void typecheck_function(Ast_Function *function);
};


#endif
