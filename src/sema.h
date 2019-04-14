
#ifndef SEMA_H
#define SEMA_H

#include "general.h"

struct Compiler;
struct Ast_Scope;
struct Ast_Expression;
struct Ast_Function;
struct Atom;

struct Sema {
    Compiler *compiler;

    Array<Ast_Scope *> scope_stack;

    Sema(Compiler *compiler) {
        this->compiler = compiler;
    }
    
    Ast_Expression *find_declaration_for_atom(Atom *atom);
    Ast_Expression *find_declaration_for_atom_in_scope(Ast_Scope *scope, Atom *atom);

    void typecheck_scope(Ast_Scope *scope);
    void typecheck_expression(Ast_Expression *expression);
    void typecheck_function(Ast_Function *function);
};


#endif
