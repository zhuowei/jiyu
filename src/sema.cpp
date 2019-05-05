
#include "sema.h"
#include "ast.h"
#include "compiler.h"

#include <stdio.h>

s32 get_levels_of_indirection(Ast_Type_Info *info) {
    s32 count = 0;
    
    while (info) {
        if (info->type == Ast_Type_Info::POINTER) {
            info = info->pointer_to;
            count++;
        } else {
            break;
        }
    }
    
    return count;
}

bool type_points_to_void_eventually(Ast_Type_Info *ptr) {
    while (ptr) {
        if (ptr->type == Ast_Type_Info::POINTER) {
            ptr = ptr->pointer_to;
        } else {
            return ptr->type == Ast_Type_Info::VOID;
        }
    }
    
    return false;
}

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
    while (expr->substitution) expr = expr->substitution;
    
    assert(expr->type_info->type == Ast_Type_Info::INTEGER);
    assert(target->type == Ast_Type_Info::INTEGER);
    
    if (target->size == expr->type_info->size) return expr;
    
    Ast_Cast *cast = new Ast_Cast();
    cast->text_span = expr->text_span;
    cast->expression = expr;
    // cast->target_type_inst = nullptr;
    cast->type_info = target;
    return cast;
}

Ast_Expression *cast_float_to_float(Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;
    
    assert(expr->type_info->type == Ast_Type_Info::FLOAT);
    assert(target->type == Ast_Type_Info::FLOAT);
    
    if (target->size == expr->type_info->size) return expr;
    
    Ast_Cast *cast = new Ast_Cast();
    cast->text_span = expr->text_span;
    cast->expression = expr;
    // cast->target_type_info = nullptr;
    cast->type_info = target;
    return cast;
}

Ast_Expression *cast_ptr_to_ptr(Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;
    
    assert(expr->type_info->type == Ast_Type_Info::POINTER);
    assert(target->type == Ast_Type_Info::POINTER);
    
    Ast_Cast *cast = new Ast_Cast();
    cast->text_span = expr->text_span;
    cast->expression = expr;
    cast->type_info = target;
    return cast;
}

bool expression_is_lvalue(Ast_Expression *expression, bool parent_wants_lvalue) {
    while (expression->substitution) expression = expression->substitution;
    
    switch (expression->type) {
        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            auto decl = static_cast<Ast_Declaration *>(ident->resolved_declaration);
            
            if (decl) assert(decl->type == AST_DECLARATION);
            
            if (decl && decl->is_let) {
                return false;
            }
            
            return decl != nullptr;
        }
        
        case AST_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            
            return expression_is_lvalue(deref->left, parent_wants_lvalue);
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
void Sema::typecheck_and_implicit_cast_expression_pair(Ast_Expression *left, Ast_Expression *right, Ast_Expression **result_left, Ast_Expression **result_right, bool allow_coerce_to_ptr_void) {
    while (left->substitution)  left  = left->substitution;
    while (right->substitution) right = right->substitution;
    
    if (left->type == AST_LITERAL) {
        typecheck_expression(right);
        
        while (right->substitution) right = right->substitution;
        
        typecheck_expression(left, right->type_info);
    } else {
        typecheck_expression(left);
        
        while (left->substitution) left = left->substitution;
        
        typecheck_expression(right, left->type_info);
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
        } else if (allow_coerce_to_ptr_void && is_pointer_type(ltype) && is_pointer_type(rtype)) {
            
            // @Note you're only allowed to coerce right-to-left here, meaning if the right-expression is *void, the left-expression cannot coerce away from whatever ptr type it is.
            if (type_points_to_void_eventually(ltype)) {
                auto left_indir = get_levels_of_indirection(ltype);
                auto right_indir = get_levels_of_indirection(rtype);
                
                if (left_indir == right_indir) {
                    right = cast_ptr_to_ptr(right, ltype);
                }
            }
        }
    }
    
    if (result_left)  *result_left  = left;
    if (result_right) *result_right = right;
}

void Sema::typecheck_scope(Ast_Scope *scope) {
    assert(scope->substitution == nullptr);
    
    Ast_Scope *last = get_current_scope();
    
    if (last && !scope->owning_function) {
        assert(last->owning_function);
        scope->owning_function = last->owning_function;
    }
    
    if (last) assert(scope->owning_function);
    
    scope_stack.add(scope);
    
    for (auto &it : scope->statements) {
        // @TODO should we do replacements at the scope level?
        typecheck_expression(it);
    }
    
    scope_stack.pop();
}

Ast_Expression *Sema::find_declaration_for_atom_in_scope(Ast_Scope *scope, Atom *atom) {
    // @Incomplete check scope tree
    for (auto it : scope->declarations) {
        while (it->substitution) it = it->substitution;
        
        if (it->type == AST_DECLARATION) {
            auto decl = static_cast<Ast_Declaration *>(it);
            if (decl->identifier->name == atom) return it;
        } else if (it->type == AST_FUNCTION) {
            auto function = static_cast<Ast_Function *>(it);
            if (function->identifier->name == atom) return function;
        } else if (it->type == AST_TYPE_ALIAS) {
            auto alias = static_cast<Ast_Type_Alias *>(it);
            if (alias->identifier->name == atom) return alias;
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


void Sema::typecheck_expression(Ast_Expression *expression, Ast_Type_Info *want_numeric_type) {
    while (expression->substitution) expression = expression->substitution;
    if (expression->type_info) return;
    
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
                typecheck_expression(decl);
                
                ident->resolved_declaration = decl;
                ident->type_info = decl->type_info;
            }
            
            return;
        }
        
        case AST_DECLARATION: {
            auto decl = static_cast<Ast_Declaration *>(expression);
            
            // @TODO prevent use of a declaration in it's initializer
            if (decl->initializer_expression) typecheck_expression(decl->initializer_expression, get_type_info(decl));
            
            if (decl->is_let && !decl->is_function_argument && !decl->initializer_expression) {
                compiler->report_error(decl, "let constant must be initialized by an expression.\n");
            }
            
            if (decl->is_let && decl->initializer_expression) {
                if (!resolves_to_literal_value(decl->initializer_expression)) {
                    compiler->report_error(decl->initializer_expression, "let constant can only be initialized by a literal expression.\n");
                }
                
                // decl->substitution = decl->initializer_expression;
            }
            
            if (decl->type_inst) {
                decl->type_info = resolve_type_inst(decl->type_inst); 
            } else {
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
                    return;
                }
            }
            
            return;
        }
        
        case AST_BINARY_EXPRESSION: {
            auto bin = static_cast<Ast_Binary_Expression *>(expression);
            
            bool allow_coerce_to_ptr_void = bin->operator_type == Token::EQUALS;
            
            typecheck_and_implicit_cast_expression_pair(bin->left, bin->right, &bin->left, &bin->right, allow_coerce_to_ptr_void);
            
            if (compiler->errors_reported) return;
            
            // @Hack @Incomplete
            bin->type_info = get_type_info(bin->left);
            
            assert(bin->type_info);
            
            
            if (bin->operator_type == Token::EQUALS) {
                if (!expression_is_lvalue(bin->left, true)) {
                    compiler->report_error(bin->left, "expression on lhs of '=' must be an lvalue.\n");
                }
            }
            
            if (bin->operator_type == Token::EQ_OP
                || bin->operator_type == Token::NE_OP
                || bin->operator_type == Token::LE_OP
                || bin->operator_type == Token::GE_OP) {
                bin->type_info = compiler->type_bool;
            }
            
            auto left_type  = get_type_info(bin->left);
            auto right_type = get_type_info(bin->right);
            if (!types_match(left_type, right_type)) {
                if ((bin->operator_type == Token::PLUS
                     || bin->operator_type == Token::MINUS) &&
                    left_type->type == Ast_Type_Info::POINTER && right_type->type == Ast_Type_Info::INTEGER) {
                    return;
                }
                
                // @TODO report types
                // @TOOD report operator
                compiler->report_error(bin, "Incompatible types found on lhs and rhs of binary operator.");
                return;
            }
            
            return;
        }
        
        case AST_UNARY_EXPRESSION: {
            auto un = static_cast<Ast_Unary_Expression *>(expression);
            typecheck_expression(un->expression);
            
            if (un->operator_type == Token::STAR) {
                if (!expression_is_lvalue(un->expression, true)) {
                    compiler->report_error(un, "lvalue required as unary '%c' operand.\n", un->operator_type);
                }
                un->type_info = make_pointer_type(get_type_info(un->expression));
                
                auto expr = un->expression;
                while (expr->substitution) expr = expr->substitution;
                
                if (expr->type == AST_UNARY_EXPRESSION) {
                    auto second = static_cast<Ast_Unary_Expression *>(expr);
                    if (second->operator_type == Token::DEREFERENCE_OR_SHIFT && expression_is_lvalue(second->expression, false)) {
                        // remove this sequence of *<< because it is ineffective.
                        un->substitution = second->expression;
                        return;
                    }
                }
            } else if (un->operator_type == Token::DEREFERENCE_OR_SHIFT) {
                auto type = get_type_info(un->expression);
                if (type->type != Ast_Type_Info::POINTER) {
                    compiler->report_error(un, "Cannot use '<<' on a non-pointer expression.\n");
                    return;
                }
                
                un->type_info = type->pointer_to;
            }
            
            assert(un->type_info);
            return;
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
            
            if (lit->literal_type == Ast_Literal::NULLPTR) {
                lit->type_info = compiler->type_ptr_void;
                if (want_numeric_type && want_numeric_type->type == Ast_Type_Info::POINTER) {
                    lit->type_info = want_numeric_type;
                }
            }
            
            assert(lit->type_info);
            
            return;
        }
        
        case AST_FUNCTION: {
            auto function = static_cast<Ast_Function *>(expression);
            typecheck_function(function);
            return;
        }
        
        case AST_FUNCTION_CALL: {
            auto call = static_cast<Ast_Function_Call *>(expression);
            typecheck_expression(call->identifier);
            
            if (compiler->errors_reported) return;
            
            assert(call->identifier->resolved_declaration);
            auto function = static_cast<Ast_Function *>(call->identifier->resolved_declaration);
            
            if (function->type != AST_FUNCTION) {
                String name = call->identifier->name->name;
                compiler->report_error(call, "Declaration '%.*s' is not a function.\n", name.length, name.data);
                return;
            }
            
            bool pass_c_varags = (function->is_c_varargs && call->argument_list.count >= function->arguments.count);
            if (!pass_c_varags &&  call->argument_list.count != function->arguments.count) {
                // @TODO print function declaration as well as call site
                compiler->report_error(call, "Mismatch in function call arguments. Wanted %lld, got %lld.\n", function->arguments.count, call->argument_list.count);
                return;
            }
            
            typecheck_function(function);
            
            // @Incomplete check that types match between arguments
            for (array_count_type i = 0; i < call->argument_list.count; ++i) {
                auto value = call->argument_list[i];
                
                if (i < function->arguments.count) {
                    // use null for morphed param because we don't care about the modified value because function parameter declarations can't be mutated here
                    auto param = function->arguments[i];
                    bool allow_coerce_to_ptr_void = true;
                    typecheck_and_implicit_cast_expression_pair(param, value, nullptr, &value, allow_coerce_to_ptr_void);
                    
                    if (!types_match(value->type_info, param->type_info)) {
                        compiler->report_error(value, "Mismatch in function call argument types.\n");
                        return;
                    }
                } else if (function->is_c_varargs) {
                    // just do a normal typecheck on the call argument since this is for varargs
                    typecheck_expression(value);
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
            
            return;
        }
        
        case AST_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            typecheck_expression(deref->left);
            
            
            assert(get_type_info(deref->left));
            
            // @Incomplete structs
            auto left_type = get_type_info(deref->left);
            if (left_type->type != Ast_Type_Info::STRING) {
                // @Incomplete report_error
                compiler->report_error(deref, "Attempt to dereference a type that is not a string or struct!\n");
                return;
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
            } else {
                compiler->report_error(deref, "No member '%.*s' in type string.\n", field_name.length, field_name.data);
            }
            
            return;
        }
        
        case AST_IF: {
            auto _if = static_cast<Ast_If *>(expression);
            
            typecheck_expression(_if->condition);
            
            if (compiler->errors_reported) return;
            
            auto cond = _if->condition;
            if (get_type_info(cond)->type != Ast_Type_Info::BOOL) {
                // @TODO check for coercion to bool
                compiler->report_error(cond, "'if' condition isn't of boolean type.\n");
            }
            
            if (_if->then_statement) typecheck_expression(_if->then_statement);
            if (_if->else_statement) typecheck_expression(_if->else_statement);
            
            return;
        }
        
        case AST_WHILE: {
            auto loop = static_cast<Ast_While *>(expression);
            
            typecheck_expression(loop->condition);
            
            if (compiler->errors_reported) return;
            
            auto cond = loop->condition;
            if (get_type_info(cond)->type != Ast_Type_Info::BOOL) {
                // @TODO check for coercion to bool
                compiler->report_error(cond, "'while' condition isn't of boolean type.\n");
            }
            
            if (loop->statement) typecheck_expression(loop->statement);
            
            return;
        }
        
        case AST_SCOPE: {
            auto scope = static_cast<Ast_Scope *>(expression);
            typecheck_scope(scope);
            scope->type_info = compiler->type_void;
            return;
        }
        
        case AST_RETURN: {
            auto ret = static_cast<Ast_Return *>(expression);
            
            auto scope = get_current_scope();
            assert(scope);
            
            auto function = scope->owning_function;
            if (function->return_decl) {
                // since we are currently in the scope of this function, return_decl should be gauranteed to be typechecked already.
                assert(get_type_info(function->return_decl));
                
                auto return_type = get_type_info(function->return_decl);
                
                if (!ret->expression) {
                    compiler->report_error(ret, "'return' statement must return an expression of function return type.\n");
                    // @Cleanup
                    print_type(return_type);
                    printf("\n");
                    return;
                }
                
                typecheck_expression(ret->expression);
                if (compiler->errors_reported) return;
                
                auto value_type = get_type_info(ret->expression);
                
                if (!types_match(value_type, return_type)) {
                    compiler->report_error(ret, "Type of return expression does not match function return type.\n");
                    // @Cleanup
                    print_type(return_type);
                    printf("\n");
                    print_type(value_type);
                    printf("\n");
                    return;
                }
            } else if (ret->expression) {
                compiler->report_error(ret, "Cannot return non-void expression in function returning void.\n");
                
                typecheck_expression(ret->expression);
            }
            
            return;
        }
        
        case AST_CAST: {
            auto cast = static_cast<Ast_Cast *>(expression);
            
            typecheck_expression(cast->expression);
            
            if (compiler->errors_reported) return;
            
            auto expr_type = get_type_info(cast->expression);
            auto target    = resolve_type_inst(cast->target_type_inst);
            
            assert(target);
            
            cast->type_info = target;
            
            if (!is_valid_primitive_cast(target, expr_type)) {
                // @TODO print the types we're trying to cast between
                compiler->report_error(cast, "Cast is invalid.\n");
            }
            
            return;
        }
        
        case AST_TYPE_ALIAS: {
            auto alias = static_cast<Ast_Type_Alias *>(expression);
            alias->type_info = compiler->type_info_type;
            return;
        }
    }
    
    assert(false);
    return;
}

Ast_Type_Info *Sema::resolve_type_inst(Ast_Type_Instantiation *type_inst) {
    if (type_inst->pointer_to) {
        auto pointee = resolve_type_inst(type_inst->pointer_to);
        return make_pointer_type(pointee);
    }
    
    if (type_inst->builtin_primitive) {
        return type_inst->builtin_primitive;
    }
    
    if (type_inst->typename_identifier) {
        typecheck_expression(type_inst->typename_identifier);
        
        if (compiler->errors_reported) return nullptr;
        
        auto ident = type_inst->typename_identifier;
        
        if (ident->type_info->type != Ast_Type_Info::TYPE) {
            String name = ident->name->name;
            compiler->report_error(ident, "Identifier '%.*s' doesn't name a type.\n", name.length, name.data);
            return nullptr;
        }
        
        auto decl = ident->resolved_declaration;
        
        if (decl->type == AST_TYPE_ALIAS) {
            auto alias = static_cast<Ast_Type_Alias *>(decl);
            
            return resolve_type_inst( alias->internal_type_inst);
        } else {
            assert(false);
            
        }}
    
    assert(false);
    return nullptr;
}

Ast_Scope *Sema::get_current_scope() {
    if (scope_stack.count) {
        return scope_stack[scope_stack.count-1];
    }
    
    return nullptr;
}

void Sema::typecheck_function(Ast_Function *function) {
    // @Incomplete set type info so we don't come through here multiple times
    
    for (auto &a : function->arguments) {
        a->is_function_argument = true;
        typecheck_expression(a);
    }
    
    if (function->return_decl) typecheck_expression(function->return_decl);
    
    if (compiler->errors_reported) return;
    
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
        for (auto a : function->arguments) {
            scope->declarations.add(a);
        }
        
        scope->statements.add(function->scope);
        scope->owning_function = function; // Set owning function here because we are the first scope in the stack beginning at this function, child scopes will inherit owning_function from the previous scope in the stack.
        
        typecheck_scope(scope);
        // typecheck_scope(function->scope);
    }
}
