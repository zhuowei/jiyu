
#include "sema.h"
#include "ast.h"
#include "compiler.h"

void Sema::typecheck_scope(Ast_Scope *scope) {
    scope_stack.add(scope);

    for (auto &it : scope->statements) {
        typecheck_expression(it);
    }

    scope_stack.pop();
}

Ast_Expression *Sema::find_declaration_for_atom_in_scope(Ast_Scope *scope, Atom *atom) {
    // @Incomplete check scope tree
    for (auto &it : scope->declarations) {
        if (it->type == AST_DECLARATION) {
            auto decl = static_cast<Ast_Declaration *>(it);
            if (decl->identifier->name == atom) return it;
        } else if (it->type == AST_FUNCTION) {
            auto function = static_cast<Ast_Function *>(it);
            if (function->identifier->name == atom) return function;
        } else {
            assert(false);
        }
    }

    return nullptr;
}

Ast_Expression *Sema::find_declaration_for_atom(Atom *atom) {
    for (auto i = scope_stack.count; i > 0; --i) {
        auto it = scope_stack[i-1];

        auto decl = find_declaration_for_atom_in_scope(it, atom);
        if (decl) return decl;
    }

    return nullptr;
}


void Sema::typecheck_expression(Ast_Expression *expression) {
    switch (expression->type) {
        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            assert(ident->name);

            auto decl = find_declaration_for_atom(ident->name);

            if (!decl) {
                String name = ident->name->name;

                // @FixMe pass in ident
                compiler->report_error(nullptr, "Undeclared identifier '%.*s'\n", name.length, name.data);
            } else {
                ident->resolved_declaration = decl;
                ident->type_info = decl->type_info;
            }
            break;
        }

        case AST_DECLARATION: {
            auto decl = static_cast<Ast_Declaration *>(expression);

            // @TODO prevent use of a declaration in it's initializer
            if (decl->initializer_expression) typecheck_expression(decl->initializer_expression);

            if (!decl->type_info) {
                assert(decl->initializer_expression);

                decl->type_info = decl->initializer_expression->type_info;
            }
            break;
        }

        case AST_BINARY_EXPRESSION: {
            auto bin = static_cast<Ast_Binary_Expression *>(expression);
            typecheck_expression(bin->left);
            typecheck_expression(bin->right);

            // @Hack @Incomplete
            bin->type_info = bin->left->type_info;
            break;
        }

        case AST_LITERAL: {
            auto lit = static_cast<Ast_Literal *>(expression);

            // @Incomplete
            lit->type_info = compiler->type_int32;
            break;
        }

        case AST_FUNCTION: {
            auto function = static_cast<Ast_Function *>(expression);
            typecheck_function(function);
            break;
        }

        case AST_FUNCTION_CALL: {
            auto call = static_cast<Ast_Function_Call *>(expression);
            typecheck_expression(call->identifier);
            if (compiler->errors_reported) return;

            assert(call->identifier->resolved_declaration);
            auto function = static_cast<Ast_Function *>(call->identifier->resolved_declaration);

            if (function->type != AST_FUNCTION) {
                String name = call->identifier->name->name;
                compiler->report_error(nullptr, "Declaration '%.*s' is not a function.\n", name.length, name.data);
                return;
            }

            if (call->argument_list.count != function->arguments.count) {
                compiler->report_error(nullptr, "Mismatch in function call arguments. Wanted %lld got %lld.\n", function->arguments.count, call->argument_list.count);
                return;
            }

            // @Incomplete check that types match between arguments
            for (auto &it : call->argument_list) {
                typecheck_expression(it);
            }

            break;
        }
    }
}

void Sema::typecheck_function(Ast_Function *function) {
    // @Incomplete typecheck arguments and return declarations

    if (function->scope) typecheck_scope(function->scope);
}
