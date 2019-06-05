
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
struct Ast_Function_Call;

struct Sema {
    Compiler *compiler;
    
    Sema(Compiler *compiler) {
        this->compiler = compiler;
    }
    
    Ast_Function *get_polymorph_for_function_call(Ast_Function *template_function, Ast_Function_Call *call);
    
    Tuple<bool, u64> function_call_is_viable(Ast_Function_Call *call, Ast_Function *function, bool do_errors);
    void collect_function_overloads_for_atom(Atom *atom, Ast_Scope *start, Array<Ast_Function *> *overload_set);
    Ast_Expression *find_declaration_for_atom(Atom *atom, Ast_Scope *start);
    Ast_Expression *find_declaration_for_atom_in_scope(Ast_Scope *scope, Atom *atom);
    
    Ast_Type_Info *resolve_type_inst(Ast_Type_Instantiation *type_inst);
    
    void typecheck_scope(Ast_Scope *scope);
    Tuple<u64, u64> typecheck_and_implicit_cast_expression_pair(Ast_Expression *left, Ast_Expression *right, Ast_Expression **result_left, Ast_Expression **result_right, bool allow_coerce_to_ptr_void);
    void typecheck_expression(Ast_Expression *expression, Ast_Type_Info *want_numeric_type = nullptr);
    void typecheck_function(Ast_Function *function);
};


#endif
