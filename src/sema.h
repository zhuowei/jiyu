
#ifndef SEMA_H
#define SEMA_H

#include "general.h"

struct Compiler;
struct Ast_Scope;
struct Ast_Expression;
struct Ast_Function;
struct Ast_Type_Info;
struct Atom;

struct Sema {
    Compiler *compiler;
    
    Array<Ast_Scope *> scope_stack;
    
    Sema(Compiler *compiler) {
        this->compiler = compiler;
    }
    
    Ast_Scope *get_current_scope();
    
    Ast_Expression *find_declaration_for_atom(Atom *atom);
    Ast_Expression *find_declaration_for_atom_in_scope(Ast_Scope *scope, Atom *atom);
    
    void typecheck_scope(Ast_Scope *scope);
    void typecheck_and_implicit_cast_expression_pair(Ast_Expression *left, Ast_Expression *right, Ast_Expression **result_left, Ast_Expression **result_right);
    void typecheck_expression(Ast_Expression *expression, Ast_Type_Info *want_numeric_type = nullptr);
    void typecheck_function(Ast_Function *function);
};


#endif
