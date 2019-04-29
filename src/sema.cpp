
#include "sema.h"
#include "ast.h"
#include "compiler.h"

#include <stdio.h>

void print_type(Ast_Type_Info *info) {
    if (info->type == Ast_Type_Info::INTEGER) {
        if (info->is_signed) {
            switch (info->size) {
                case 1: printf("int8"); return;
                case 2: printf("int16"); return;
                case 4: printf("int32"); return;
                case 8: printf("int64"); return;
                default: assert(false);
            }
        } else {
            switch (info->size) {
                case 1: printf("uint8"); return;
                case 2: printf("uint16"); return;
                case 4: printf("uint32"); return;
                case 8: printf("uint64"); return;
                default: assert(false);
            }
        }
    }
    
    if (info->type == Ast_Type_Info::BOOL) {
        printf("bool");
        return;
    }
    
    if (info->type == Ast_Type_Info::FLOAT) {
        if (info->size == 4) {
            printf("float");
        } else {
            assert(info->size == 8);
            printf("double");
        }
        return;
    }
    
    if (info->type == Ast_Type_Info::POINTER) {
        printf("*");
        print_type(info->pointer_to);
        return;
    }
    
    if (info->type == Ast_Type_Info::STRING) {
        printf("string");
        return;
    }
    
    assert(false);
}

Ast_Expression *cast_int_to_int(Ast_Expression *expr, Ast_Type_Info *target) {
    assert(expr->type_info->type == Ast_Type_Info::INTEGER);
    assert(target->type == Ast_Type_Info::INTEGER);
    
    if (target->size == expr->type_info->size) return expr;
    
    Ast_Cast *cast = new Ast_Cast();
    cast->expression = expr;
    cast->type_info = target;
    return cast;
}

Ast_Expression *cast_float_to_float(Ast_Expression *expr, Ast_Type_Info *target) {
    assert(expr->type_info->type == Ast_Type_Info::FLOAT);
    assert(target->type == Ast_Type_Info::FLOAT);
    
    if (target->size == expr->type_info->size) return expr;
    
    Ast_Cast *cast = new Ast_Cast();
    cast->expression = expr;
    cast->type_info = target;
    return cast;
}

bool expression_is_lvalue(Ast_Expression *expression, bool parent_wants_lvalue) {
    switch (expression->type) {
        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            auto decl = ident->resolved_declaration;
            
            // @Incomplete return false for constant declarations
            
            return decl != nullptr;
        }
        
        case AST_UNARY_EXPRESSION: {
            auto un = static_cast<Ast_Unary_Expression *>(expression);
            if (un->operator_type == Token::STAR) {
                auto expr = expression_is_lvalue(un->expression, true);
                return !expr; // I think this is correct, but I havent thought about it deeply -josh 18 April 2019
            } else if (un->operator_type == Token::DEREFERENCE_OR_SHIFT) {
                auto expr = expression_is_lvalue(un->expression, false);
                if (parent_wants_lvalue && expr) return true;
                return false; // I think this is correct, but I havent thought about it deeply -josh 18 April 2019
            } else {
                assert(false);
            }
        }
    }
    
    return false;
}

// @Cleanup if we introduce expression substitution, then we can remove the resut parameters and just set (left/right)->substitution
// actually, if we do that, then we can't really use this for checking function calls.
void Sema::typecheck_and_implicit_cast_expression_pair(Ast_Expression *left, Ast_Expression *right, Ast_Expression **result_left, Ast_Expression **result_right) {
    if (left->type == AST_LITERAL) {
        right = typecheck_expression(right);
        left  = typecheck_expression(left, right->type_info);
    } else {
        left  = typecheck_expression(left);
        right = typecheck_expression(right, left->type_info);
    }
    
    if (compiler->errors_reported) return;
    
    assert(left->type_info);
    assert(right->type_info);
    
    auto ltype = left->type_info;
    auto rtype = right->type_info;
    if (!types_match(ltype, rtype)) {
        if (is_int_type(ltype) && is_int_type(rtype) && (ltype->is_signed == rtype->is_signed)) {
            if (ltype->size < rtype->size) {
                left = cast_int_to_int(left, rtype);
            } else if (ltype->size > rtype->size) {
                right = cast_int_to_int(right, ltype);
            }
        } else if (is_float_type(ltype) && is_float_type(rtype)) {
            if (ltype->size < rtype->size) {
                left = cast_float_to_float(left, rtype);
            } else if (ltype->size > rtype->size) {
                right = cast_float_to_float(right, ltype);
            }
        }
    }
    
    if (result_left)  *result_left  = left;
    if (result_right) *result_right = right;
}

void Sema::typecheck_scope(Ast_Scope *scope) {
    scope_stack.add(scope);
    
    for (auto &it : scope->statements) {
        // @TODO should we do replacements at the scope level?
        auto _ = typecheck_expression(it);
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


Ast_Expression *Sema::typecheck_expression(Ast_Expression *expression, Ast_Type_Info *want_numeric_type) {
    switch (expression->type) {
        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            assert(ident->name);
            
            auto decl = find_declaration_for_atom(ident->name);
            
            if (!decl) {
                String name = ident->name->name;
                
                // @FixMe pass in ident
                compiler->report_error(ident, "Undeclared identifier '%.*s'\n", name.length, name.data);
            } else {
                ident->resolved_declaration = decl;
                ident->type_info = decl->type_info;
            }
            
            return ident;
        }
        
        case AST_DECLARATION: {
            auto decl = static_cast<Ast_Declaration *>(expression);
            
            // @TODO prevent use of a declaration in it's initializer
            if (decl->initializer_expression) decl->initializer_expression = typecheck_expression(decl->initializer_expression, decl->type_info);
            
            if (!decl->type_info) {
                assert(decl->initializer_expression);
                
                decl->type_info = decl->initializer_expression->type_info;
            }
            
            if (decl->type_info && decl->initializer_expression) {
                if (!types_match(decl->type_info, decl->initializer_expression->type_info)) {
                    // @TODO report the types here
                    // @TODO attempt to implciit cast if available
                    compiler->report_error(decl->initializer_expression, "Attempt to initialize variable with expression of incompatible type.\n");
                    print_type(decl->type_info);
                    printf("\n");
                    print_type(decl->initializer_expression->type_info);
                    printf("\n");
                    return decl;
                }
            }
            
            return decl;
        }
        
        case AST_BINARY_EXPRESSION: {
            auto bin = static_cast<Ast_Binary_Expression *>(expression);
            
            typecheck_and_implicit_cast_expression_pair(bin->left, bin->right, &bin->left, &bin->right);
            
            if (compiler->errors_reported) return bin;
            
            // @Hack @Incomplete
            bin->type_info = bin->left->type_info;
            
            if (bin->operator_type == Token::EQ_OP
                || bin->operator_type == Token::NE_OP
                || bin->operator_type == Token::LE_OP
                || bin->operator_type == Token::GE_OP) {
                bin->type_info = compiler->type_bool;
            }
            
            if (!types_match(bin->left->type_info, bin->right->type_info)) {
                // @TODO report types
                // @TODO attempt to implicit cast
                // @TOOD report operator
                compiler->report_error(bin, "Incompatible types found on lhs and rhs of binary operator.");
                return bin;
            }
            
            return bin;
        }
        
        case AST_UNARY_EXPRESSION: {
            auto un = static_cast<Ast_Unary_Expression *>(expression);
            un->expression = typecheck_expression(un->expression);
            
            if (un->operator_type == Token::STAR) {
                if (!expression_is_lvalue(un->expression, true)) {
                    compiler->report_error(un, "lvalue required as unary '%c' operand.\n", un->operator_type);
                }
                un->type_info = make_pointer_type(un->expression->type_info);
                
                if (un->expression->type == AST_UNARY_EXPRESSION) {
                    auto second = static_cast<Ast_Unary_Expression *>(un->expression);
                    if (second->operator_type == Token::DEREFERENCE_OR_SHIFT && expression_is_lvalue(second->expression, false)) {
                        // remove this sequence of *<< because it is ineffective.
                        return second->expression;
                    }
                }
            } else if (un->operator_type == Token::DEREFERENCE_OR_SHIFT) {
                auto type = un->expression->type_info;
                if (type->type != Ast_Type_Info::POINTER) {
                    compiler->report_error(un, "Cannot use '<<' on a non-pointer expression.\n");
                    return un;
                }
                
                un->type_info = type->pointer_to;
            }
            
            assert(un->type_info);
            return un;
        }
        
        case AST_LITERAL: {
            auto lit = static_cast<Ast_Literal *>(expression);
            
            // @Incomplete
            
            // @Incomplete if we have a float literal but want an int type, keep a float type and let the implicit cast system do its job
            if (lit->literal_type == Ast_Literal::INTEGER) {
                if (want_numeric_type && (want_numeric_type->type == Ast_Type_Info::INTEGER || want_numeric_type->type == Ast_Type_Info::FLOAT)) {
                    // @Incomplete check that number can fit in target type
                    // @Incomplete cast to float if we have an int literal
                    lit->type_info = want_numeric_type;
                    
                    
                    // @Cleanup I'm mutating the literal for now, but this would be a good place to use substitution, I think
                    // Or since literal ints are considered completely typeless up until this point, maybe this is the right thing to do
                    if (want_numeric_type->type == Ast_Type_Info::FLOAT) {
                        lit->literal_type = Ast_Literal::FLOAT;
                        // @Cleanup the u64 cast
                        lit->float_value = static_cast<double>((u64)lit->integer_value);
                    }
                } else {
                    lit->type_info = compiler->type_int32;
                }
            }
            
            if (lit->literal_type == Ast_Literal::STRING)  lit->type_info = compiler->type_string;
            
            if (lit->literal_type == Ast_Literal::BOOL) lit->type_info = compiler->type_bool;
            
            assert(lit->type_info);
            
            return lit;
        }
        
        case AST_FUNCTION: {
            auto function = static_cast<Ast_Function *>(expression);
            typecheck_function(function);
            return function;
        }
        
        case AST_FUNCTION_CALL: {
            auto call = static_cast<Ast_Function_Call *>(expression);
            auto _ = typecheck_expression(call->identifier);
            
            if (compiler->errors_reported) return nullptr;
            
            assert(call->identifier->resolved_declaration);
            auto function = static_cast<Ast_Function *>(call->identifier->resolved_declaration);
            
            if (function->type != AST_FUNCTION) {
                String name = call->identifier->name->name;
                compiler->report_error(call, "Declaration '%.*s' is not a function.\n", name.length, name.data);
                return call;
            }
            
            bool pass_c_varags = (function->is_c_varargs && call->argument_list.count >= function->arguments.count);
            if (!pass_c_varags &&  call->argument_list.count != function->arguments.count) {
                // @TODO print function declaration as well as call site
                compiler->report_error(call, "Mismatch in function call arguments. Wanted %lld, got %lld.\n", function->arguments.count, call->argument_list.count);
                return call;
            }
            
            typecheck_function(function);
            
            // @Incomplete check that types match between arguments
            for (array_count_type i = 0; i < call->argument_list.count; ++i) {
                auto value = call->argument_list[i];
                
                if (i < function->arguments.count) {
                    // use null for morphed param because we don't care about the modified value because function parameter declarations can't be mutated here
                    auto param = function->arguments[i];
                    typecheck_and_implicit_cast_expression_pair(param, value, nullptr, &value);
                    
                    if (!types_match(value->type_info, param->type_info)) {
                        compiler->report_error(value, "Mismatch in function call argument types.\n");
                        return call;
                    }
                } else if (function->is_c_varargs) {
                    // just do a normal typecheck on the call argument since this is for varargs
                    value = typecheck_expression(value);
                } else {
                    assert(false);
                }
                
                // set value into the list in case it got implicitly cast
                call->argument_list[i] = value;
            }
            
            if (function->return_decl) {
                call->type_info = function->return_decl->type_info;
            } else {
                call->type_info = compiler->type_void;
            }
            
            return call;
        }
        
        case AST_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            deref->left = typecheck_expression(deref->left);
            
            
            assert(deref->left->type_info);
            
            // @Incomplete structs
            auto left_type = deref->left->type_info;
            if (left_type->type != Ast_Type_Info::STRING) {
                // @Incomplete report_error
                compiler->report_error(deref, "Attempt to dereference a type that is not a string or struct!\n");
                return deref;
            }
            
            // @Hack until we have field members in the type_info (data and length would be field members of string)
            assert(deref->field_selector && deref->field_selector->name);
            String field_name = deref->field_selector->name->name;
            
            if (field_name == to_string("data")) {
                deref->element_path_index = 0;
                deref->byte_offset = 0;
                
                // @Hack @Incomplete
                deref->type_info = compiler->type_string_data;
            } else if (field_name == to_string("length")) {
                deref->element_path_index = 1;
                // @Hack @Cleanup
                // @Hack @Cleanup
                // @Hack @Cleanup
                deref->byte_offset = 8; // @TargetInfo
                deref->type_info = compiler->type_string_length;
            }
            return deref;
        }
        
        case AST_IF: {
            auto _if = static_cast<Ast_If *>(expression);
            
            typecheck_expression(_if->condition);
            
            auto cond = _if->condition;
            if (cond->type_info->type != Ast_Type_Info::BOOL) {
                // @TODO check for coercion to bool
                compiler->report_error(cond, "'if' condition isn't of boolean type.\n");
            }
            
            if (_if->then_statement) typecheck_expression(_if->then_statement);
            if (_if->else_statement) typecheck_expression(_if->else_statement);
            
            return _if;
        }
        
        case AST_WHILE: {
            auto loop = static_cast<Ast_While *>(expression);
            
            typecheck_expression(loop->condition);
            
            auto cond = loop->condition;
            if (cond->type_info->type != Ast_Type_Info::BOOL) {
                // @TODO check for coercion to bool
                compiler->report_error(cond, "'while' condition isn't of boolean type.\n");
            }
            
            if (loop->statement) typecheck_expression(loop->statement);
            
            return loop;
        }
        
        case AST_SCOPE: {
            auto scope = static_cast<Ast_Scope *>(expression);
            typecheck_scope(scope);
            return scope;
        }
        
        case AST_CAST: {
            assert(false); // @Incomplete
            return nullptr;
        }
    }
    
    assert(false);
    return nullptr;
}

void Sema::typecheck_function(Ast_Function *function) {
    // @Incomplete set type info so we don't come through here multiple times
    
    for (auto &a : function->arguments) {
        typecheck_expression(a);
    }
    
    if (function->return_decl) typecheck_expression(function->return_decl);
    
    if (function->is_c_varargs && !function->is_c_function) {
        compiler->report_error(function, "Function must be tagged @c_function in order to use temporary_c_vararg.\n");
    }
    
    if (function->is_c_function) {
        // @TODO error if a C function is declared returning a tuple
        /*
        if (function->returns.count > 1) {
            compiler->report_error(function, "Function tagged @c_function may only have 1 return value.\n");
        }
*/
    }
    
    if (function->is_c_function && function->scope) {
        compiler->report_error(function, "Function marked @c_function cannot have a body.\n");
        return;
    }
    
    if (function->scope) {
        // I dont yet know if this is the best way to handle this, but we create a stand-in scope with
        // parameter declarations so the typechecker can find semantic relationships for the function
        // parameters. -josh 28 April 2019
        
        Ast_Scope *scope = new Ast_Scope(); // @Leak
        scope->text_span = function->scope->text_span;
        
        // @TODO should we add the parameters as statements too?
        for (auto &a : function->arguments) {
            scope->declarations.add(a);
        }
        
        scope->statements.add(function->scope);
        
        typecheck_scope(scope);
        // typecheck_scope(function->scope);
    }
}
