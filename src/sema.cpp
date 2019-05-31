
#include "sema.h"
#include "ast.h"
#include "compiler.h"

#include <stdio.h>

void add_type(String_Builder *builder, Ast_Type_Info *type) {
    if (type->type == Ast_Type_Info::INTEGER) {
        if (type->is_signed) builder->putchar('s');
        else                 builder->putchar('u');
        
        auto size = type->size;
        assert(size == 1 || size == 2 || size == 4 || size == 8);
        builder->print("%d", size);
    } else if (type->type == Ast_Type_Info::FLOAT) {
        if      (type->size == 4) builder->putchar('f');
        else if (type->size == 8) builder->putchar('F');
        else assert(false);
    } else if (type->type == Ast_Type_Info::VOID) {
        builder->putchar('v');
    } else if (type->type == Ast_Type_Info::BOOL) {
        builder->putchar('b');
    } else if (type->type == Ast_Type_Info::STRING) {
        builder->putchar('s');
    } else if (type->type == Ast_Type_Info::POINTER) {
        builder->putchar('p');
        add_type(builder, type->pointer_to);
    } else if (type->type == Ast_Type_Info::ALIAS) {
        add_type(builder, type->alias_of);
    } else if (type->type == Ast_Type_Info::ARRAY) {
        builder->putchar('A');
        
        if (type->array_element_count >= 0) {
            builder->putchar('k');
            builder->print("%d", type->array_element_count);
        } else if (type->is_dynamic) {
            builder->putchar('d');
        } else {
            builder->putchar('s');
        }
        
        
        builder->putchar('_');
        add_type(builder, type->array_element);
        builder->putchar('_');
    } else if (type->type == Ast_Type_Info::STRUCT) {
        builder->putchar('S');
        
        // @Incomplete structs that are declared with other structs/named-scopes.
        // @Incomplete anonymous structs?
        String name = type->struct_decl->identifier->name->name;
        builder->print("%d%.*s", name.length, name.length, name.data);
    } else {
        assert(false && "Internal error: unhandled type when creating function mangled name.");
    }
}

String get_mangled_name(Compiler *compiler, Ast_Function *function) {
    if (function->identifier->name == compiler->atom_main) return function->identifier->name->name;
    String_Builder builder;
    
    builder.append("_H");
    
    String name = function->identifier->name->name;
    builder.print("%d%.*s", name.length, name.length, name.data);
    
    builder.putchar('_');
    
    for (auto arg: function->arguments) {
        auto type = get_type_info(arg);
        add_type(&builder, type);
    }
    
    return builder.to_string();
}

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

bool type_is_iterable(Ast_Type_Info *info) {
    if (info->type == Ast_Type_Info::ARRAY) return true;
    
    // @Incomplete test for structs containing .count member and supports [] overloading.
    
    return false;
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

Ast_Expression *cast_int_to_float(Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;
    
    assert(expr->type_info->type == Ast_Type_Info::INTEGER);
    assert(target->type == Ast_Type_Info::FLOAT);
    
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


Ast_Literal *make_integer_literal(s64 value, Ast_Type_Info *type_info) {
    Ast_Literal *lit = new Ast_Literal();
    lit->literal_type = Ast_Literal::INTEGER;
    lit->integer_value = value;
    lit->type_info = type_info;
    return lit;
}


Ast_Identifier *make_identifier(Atom *name) {
    Ast_Identifier *ident = new Ast_Identifier();
    ident->name = name;
    return ident;
}

// @Note MUST typecheck the return value of this!!!
Ast_Array_Dereference *make_array_index(Ast_Expression *array, Ast_Expression *index) {
    Ast_Array_Dereference *deref = new Ast_Array_Dereference();
    deref->array_or_pointer_expression = array;
    deref->index_expression = index;
    return deref;
}

// @Note MUST typecheck the return value of this!!!
Ast_Dereference *make_derefence(Ast_Expression *aggregate_expression, Atom *field) {
    auto ident = make_identifier(field);
    ident->text_span = aggregate_expression->text_span;
    
    Ast_Dereference *deref = new Ast_Dereference();
    deref->text_span = aggregate_expression->text_span;
    deref->left = aggregate_expression;
    deref->field_selector = ident;
    return deref;
}

// @Note MUST typecheck the return value of this!!!
Ast_Unary_Expression *make_unary(Token::Type op, Ast_Expression *subexpr) {
    Ast_Unary_Expression *un = new Ast_Unary_Expression();
    un->operator_type = op;
    un->expression = subexpr;
    return un;
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
        
        case AST_ARRAY_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            // @Incomplete this isnt true if array_or_pointer_expression is a literal.
            // but maybe that never happens here due to substitution ?
            
            return true;
        }
        
        case AST_UNARY_EXPRESSION: {
            auto un = static_cast<Ast_Unary_Expression *>(expression);
            if (un->operator_type == Token::STAR) {
                auto expr = expression_is_lvalue(un->expression, true);
                return expr; // I think this is correct, but I havent thought about it deeply -josh 18 April 2019
            } else if (un->operator_type == Token::DEREFERENCE_OR_SHIFT) {
                auto expr = expression_is_lvalue(un->expression, false);
                if (parent_wants_lvalue) return true;
                return expr; // I think this is correct, but I havent thought about it deeply -josh 18 April 2019
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
    if (left->type == AST_LITERAL) {
        typecheck_expression(right);
        typecheck_expression(left, get_type_info(right));
    } else {
        typecheck_expression(left);
        typecheck_expression(right, get_type_info(left));
    }
    
    if (compiler->errors_reported) return;
    
    while (left->substitution)  left  = left->substitution;
    while (right->substitution) right = right->substitution;
    
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
        } else if (is_float_type(ltype) && is_int_type(rtype)) {
            right = cast_int_to_float(right, ltype);
        } else if (is_int_type(ltype) && is_float_type(rtype)) {
            left = cast_int_to_float(left, rtype);
        }else if (allow_coerce_to_ptr_void && is_pointer_type(ltype) && is_pointer_type(rtype)) {
            
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
    
    for (auto &it : scope->statements) {
        // @TODO should we do replacements at the scope level?
        typecheck_expression(it);
    }
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
        } else if (it->type == AST_STRUCT) {
            auto _struct = static_cast<Ast_Struct *>(it);
            if (_struct->identifier->name == atom) return _struct;
        } else {
            assert(false);
        }
    }
    
    return nullptr;
}

Ast_Expression *Sema::find_declaration_for_atom(Atom *atom, Ast_Scope *start) {
    
    while (start) {
        auto decl = find_declaration_for_atom_in_scope(start, atom);
        if (decl) return decl;
        
        start = start->parent;
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
            
            auto decl = find_declaration_for_atom(ident->name, ident->enclosing_scope);
            
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
            
            if (decl->is_let && !decl->is_readonly_variable && !decl->initializer_expression) {
                compiler->report_error(decl, "let constant must be initialized by an expression.\n");
            }
            
            if (decl->is_let && !decl->is_readonly_variable && decl->initializer_expression) {
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
                bool allow_coerce_to_ptr_void = true;
                typecheck_and_implicit_cast_expression_pair(decl, decl->initializer_expression, nullptr, &decl->initializer_expression, allow_coerce_to_ptr_void);
                if (!types_match(get_type_info(decl), get_type_info(decl->initializer_expression))) {
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
                || bin->operator_type == Token::GE_OP
                || bin->operator_type == '>'
                || bin->operator_type == '<') {
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
            } else if (un->operator_type == Token::MINUS) {
                auto type = get_type_info(un->expression);
                if (type->type != Ast_Type_Info::INTEGER && type->type != Ast_Type_Info::FLOAT) {
                    compiler->report_error(un, "Unary '-' is only valid for integer for float operands.\n");
                    return;
                }
                
                auto lit = resolves_to_literal_value(un->expression);
                // @TODO this isnt exactly correct...
                /*
                if (lit) {
                if (type->type == Ast_Type_Info::INTEGER) {
                lit->integer_value = (-lit->integer_value);
                } else if (type->type == Ast_Type_Info::FLOAT) {
                lit->float_value = (-lit->float_value);
                } else assert(0);
                
                un->substitution = lit;
                }
                */
                
                // @Incomplete I think, should we warn about unary minus on unsiged integers?
                un->type_info = type;
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
                    
                    if (compiler->errors_reported) return;
                    
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
            
            if (get_type_info(deref->left)->type == Ast_Type_Info::POINTER) {
                // we allow you to dereference once through a pointer
                // here we insert some desugaring that expands pointer.field into (<<pointer).field
                
                auto un = make_unary(Token::DEREFERENCE_OR_SHIFT, deref->left);
                un->text_span = deref->left->text_span;
                
                typecheck_expression(un);
                deref->left = un; // Dont set substitution here, otherwise we'll infinite loop
            }
            
            
            assert(get_type_info(deref->left));
            
            // @Incomplete structs
            auto left_type = get_type_info(deref->left);
            if (left_type->type != Ast_Type_Info::STRING &&
                left_type->type != Ast_Type_Info::ARRAY  &&
                left_type->type != Ast_Type_Info::STRUCT) {
                // @Incomplete report_error
                compiler->report_error(deref, "Attempt to dereference a type that is not a string, struct, or array!\n");
                return;
            }
            
            // @Hack until we have field members in the type_info (data and length would be field members of string)
            assert(deref->field_selector && deref->field_selector->name);
            Atom *field_atom = deref->field_selector->name;
            
            if (left_type->type == Ast_Type_Info::STRING) {
                if (field_atom == compiler->atom_data) {
                    deref->element_path_index = 0;
                    deref->byte_offset = 0;
                    
                    // @Hack @Incomplete
                    deref->type_info = compiler->type_string_data;
                } else if (field_atom == compiler->atom_length) {
                    deref->element_path_index = 1;
                    // @Hack @Cleanup
                    // @Hack @Cleanup
                    // @Hack @Cleanup
                    deref->byte_offset = 8; // @TargetInfo
                    deref->type_info = compiler->type_string_length;
                } else {
                    String field_name = field_atom->name;
                    compiler->report_error(deref, "No member '%.*s' in type string.\n", field_name.length, field_name.data);
                }
            } else if (left_type->type == Ast_Type_Info::ARRAY) {
                if (left_type->array_element_count == -1) {
                    if (field_atom == compiler->atom_data) {
                        deref->element_path_index = 0;
                        deref->byte_offset = 0;
                        
                        // @Hack @Incomplete
                        deref->type_info = make_pointer_type(left_type->array_element);
                    } else if (field_atom == compiler->atom_count) {
                        deref->element_path_index = 1;
                        // @Hack @Cleanup
                        // @Hack @Cleanup
                        // @Hack @Cleanup
                        deref->byte_offset = 8; // @TargetInfo
                        deref->type_info = compiler->type_array_count;
                    } else if (left_type->is_dynamic && field_atom == compiler->atom_allocated) {
                        deref->element_path_index = 2;
                        // @Hack @Cleanup
                        // @Hack @Cleanup
                        // @Hack @Cleanup
                        deref->byte_offset = 16; // @TargetInfo
                        deref->type_info = compiler->type_array_count;
                    } else {
                        String field_name = field_atom->name;
                        compiler->report_error(deref, "No member '%.*s' in type array.\n", field_name.length, field_name.data);
                    }
                } else {
                    assert(left_type->is_dynamic == false);
                    
                    if (field_atom == compiler->atom_data) {
                        auto index_lit = make_integer_literal(0, compiler->type_array_count);
                        index_lit->text_span = deref->text_span;
                        
                        auto index = make_array_index(deref->left, index_lit);
                        index->text_span = deref->text_span;
                        
                        auto addr = make_unary(Token::STAR, index);
                        addr->text_span = deref->text_span;
                        
                        typecheck_expression(addr);
                        deref->substitution = addr;
                    } else if (field_atom == compiler->atom_count) {
                        auto lit = make_integer_literal(left_type->array_element_count, compiler->type_array_count);
                        lit->text_span = deref->text_span;
                        deref->substitution = lit;
                    } else {
                        String field_name = field_atom->name;
                        compiler->report_error(deref, "No member '%.*s' in known-size array.\n", field_name.length, field_name.data);
                    }
                }
            } else if (left_type->type == Ast_Type_Info::STRUCT) {
                s64 element_index = 0;
                bool found = false;
                for (auto member : left_type->struct_members) {
                    if (member.is_let) continue;
                    
                    if (member.name == field_atom) {
                        found = true;
                        
                        deref->element_path_index = element_index;
                        deref->type_info = member.type_info;
                        deref->byte_offset = -1; // @Incomplete
                        break;
                    }
                    
                    element_index += 1;
                }
                
                if (!found) {
                    String field_name = field_atom->name;
                    String name = left_type->struct_decl->identifier->name->name;
                    compiler->report_error(deref, "No member '%.*s' in struct %.*s.\n", field_name.length, field_name.data, name.length, name.data);
                }
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
        
        case AST_FOR: {
            auto _for = static_cast<Ast_For *>(expression);
            if (!_for->initial_iterator_expression) {
                compiler->report_error(_for, "'for' must be followed by an expression.\n");
                return;
            }
            
            typecheck_expression(_for->initial_iterator_expression);
            
            if (compiler->errors_reported) return;
            
            if (!_for->upper_range_expression) {
                auto init_type = get_type_info(_for->initial_iterator_expression);
                if (init_type->type == Ast_Type_Info::INTEGER) {
                    compiler->report_error(_for, "'for' must specify an upper-range. Ex: for 0..1\n");
                    return;
                }
            } else {
                auto init_type = get_type_info(_for->initial_iterator_expression);
                if (init_type->type != Ast_Type_Info::INTEGER) {
                    compiler->report_error(_for, "'..' operator may only be preceeded by an integer expression.\n");
                    return;
                }
                
                typecheck_expression(_for->upper_range_expression);
                if (compiler->errors_reported) return;
                
                auto init_expr = _for->initial_iterator_expression;
                auto upper_expr = _for->upper_range_expression;
                bool allow_coerce_to_ptr_void = false;
                typecheck_and_implicit_cast_expression_pair(init_expr, upper_expr, &_for->initial_iterator_expression, &_for->upper_range_expression, allow_coerce_to_ptr_void);
                
                init_type = get_type_info(_for->initial_iterator_expression);
                auto upper_type = get_type_info(_for->upper_range_expression);
                if (upper_type->type != Ast_Type_Info::INTEGER) {
                    compiler->report_error(_for->upper_range_expression, "'for' upper-range must be an integer expression.\n");
                    return;
                }
                
                if (!types_match(init_type, upper_type)) {
                    compiler->report_error(_for, "'for' lower-range and upper-range types do not match!\n");
                }
            }
            
            auto init_type = get_type_info(_for->initial_iterator_expression);
            if (!is_int_type(init_type)) {
                // @Incomplete
                bool supports_iteration_interface = type_is_iterable(init_type);
                
                if (!supports_iteration_interface) {
                    compiler->report_error(_for->initial_iterator_expression, "Type of expression in 'for' condition is not iterable. Must be an integer range or a type that supports the iteration-inteface (.count, []).");
                    return;
                }
            }
            
            if (!is_int_type(init_type)) {
                auto count_expr = make_derefence(_for->initial_iterator_expression, compiler->atom_count);
                typecheck_expression(count_expr);
                
                auto zero = make_integer_literal(0, get_type_info(count_expr));
                auto it_index_ident = make_identifier(compiler->atom_it_index);
                it_index_ident->text_span = _for->text_span;
                it_index_ident->enclosing_scope = &_for->body;
                {
                    Ast_Declaration *decl = new Ast_Declaration();
                    decl->text_span  = _for->text_span;
                    decl->identifier = it_index_ident;
                    decl->identifier->text_span = _for->text_span;
                    decl->initializer_expression = zero;
                    decl->is_let = true;
                    decl->is_readonly_variable = true;
                    
                    _for->iterator_index_decl = decl;
                    
                    typecheck_expression(_for->iterator_index_decl);
                    if (compiler->errors_reported) return;
                }
                
                {
                    auto indexed = make_array_index(_for->initial_iterator_expression, it_index_ident);
                    
                    Ast_Declaration *decl = new Ast_Declaration();
                    decl->text_span  = _for->text_span;
                    decl->identifier = make_identifier(compiler->atom_it);
                    decl->initializer_expression = indexed;
                    decl->is_let = true;
                    decl->is_readonly_variable = true;
                    // decl->type_info = get_type_info(indexed);
                    
                    _for->iterator_decl = decl;
                }
                
                assert(_for->upper_range_expression == nullptr);
                _for->upper_range_expression = count_expr;
            }
            
            if (!_for->iterator_decl) {
                // for integer ranges only
                Ast_Declaration *decl = new Ast_Declaration();
                decl->text_span  = _for->text_span;
                decl->identifier = make_identifier(compiler->atom_it);
                decl->identifier->text_span = _for->text_span;
                decl->identifier->enclosing_scope = &_for->body;
                decl->initializer_expression = _for->initial_iterator_expression;
                decl->is_let = true;
                decl->is_readonly_variable = true;
                
                _for->iterator_decl = decl;
            }
            
            // create a temporary scope for the iterator variable
            Ast_Scope *scope = &_for->iterator_declaration_scope;
            assert(_for->iterator_decl);
            scope->declarations.add(_for->iterator_decl);
            if (_for->iterator_index_decl) scope->declarations.add(_for->iterator_index_decl);
            
            typecheck_expression(_for->iterator_decl);
            if (compiler->errors_reported) return;
            
            assert(get_type_info(_for->iterator_decl));
            
            typecheck_expression(&_for->body);
            
            if (compiler->errors_reported) return;
            
            _for->type_info = compiler->type_void;
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
            
            auto function = ret->owning_function;
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
            resolve_type_inst(alias->internal_type_inst);
            alias->type_info = compiler->type_info_type;
            return;
        }
        
        case AST_STRUCT: {
            auto _struct = static_cast<Ast_Struct *>(expression);
            typecheck_scope(&_struct->member_scope);
            _struct->type_value = make_struct_type(_struct);
            _struct->type_info = compiler->type_info_type;
            return;
        }
        
        case AST_ARRAY_DEREFERENCE: {
            auto deref = static_cast<Ast_Array_Dereference *>(expression);
            
            assert(deref->array_or_pointer_expression);
            
            if (!deref->index_expression) {
                compiler->report_error(deref, "Array index expression missing subscript.\n");
                return;
            }
            
            typecheck_expression(deref->array_or_pointer_expression);
            if (compiler->errors_reported) return;
            
            typecheck_expression(deref->index_expression);
            if (compiler->errors_reported) return;
            
            auto array_type = get_type_info(deref->array_or_pointer_expression);
            if (array_type->type != Ast_Type_Info::ARRAY && array_type->type != Ast_Type_Info::POINTER) {
                compiler->report_error(deref->array_or_pointer_expression, "Expected array or pointer for index expression, but got something else.\n");
                return;
            }
            
            auto index_type = get_type_info(deref->index_expression);
            if (index_type->type != Ast_Type_Info::INTEGER) {
                compiler->report_error(deref->index_expression, "Array index subscript must be of integer type.\n");
                return;
            }
            
            if (array_type->type == Ast_Type_Info::ARRAY &&
                array_type->array_element_count >= 0) {
                auto lit = resolves_to_literal_value(deref->index_expression);
                if (lit) {
                    auto lit_type = get_type_info(lit);
                    auto value = lit->integer_value;
                    
                    bool out_of_range = false;
                    if (lit_type->is_signed) {
                        if (value < 0) out_of_range = true;
                    }
                    
                    if (value >= array_type->array_element_count) out_of_range = true;
                    
                    if (out_of_range) {
                        compiler->report_error(deref->index_expression, "Index value %lld is outside the range of known-size array (size: %lld).\n", value, array_type->array_element_count);
                    }
                }
            }
            
            if (array_type->type == Ast_Type_Info::ARRAY) deref->type_info = array_type->array_element;
            else if (array_type->type == Ast_Type_Info::POINTER) deref->type_info = array_type->pointer_to;
            else assert(false);
            
            return;
        }
        
        case AST_SIZEOF: {
            Ast_Sizeof *size = static_cast<Ast_Sizeof *>(expression);
            
            if (!size->target_type_inst) {
                compiler->report_error(size, "sizeof() must specify a type to take the size of.\n");
                return;
            }
            
            auto type = resolve_type_inst(size->target_type_inst);
            size->type_info = type;
            
            auto lit = make_integer_literal(type->size, compiler->type_int32);
            lit->text_span = size->text_span;
            size->substitution = lit;
            return;
        }
    }
    
    assert(false);
    return;
}

Ast_Type_Info *Sema::resolve_type_inst(Ast_Type_Instantiation *type_inst) {
    if (type_inst->type_value) return type_inst->type_value;
    
    if (type_inst->pointer_to) {
        auto pointee = resolve_type_inst(type_inst->pointer_to);
        type_inst->type_value = make_pointer_type(pointee);
        return type_inst->type_value;
    }
    
    if (type_inst->builtin_primitive) {
        type_inst->type_value = type_inst->builtin_primitive;
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
            typecheck_expression(alias);
            type_inst->type_value = resolve_type_inst(alias->internal_type_inst);
            return type_inst->type_value;
        } else if (decl->type == AST_STRUCT) {
            auto _struct = static_cast<Ast_Struct *>(decl);
            typecheck_expression(_struct);
            return _struct->type_value;
        } else {
            assert(false);
        }
    }
    
    if (type_inst->array_element_type) {
        auto element = resolve_type_inst(type_inst->array_element_type);
        if (!element) {
            compiler->report_error(type_inst, "Internal error: Array-type must specify an element type.\n");
            return nullptr;
        }
        
        if (type_inst->array_size_expression) {
            auto size_expr = type_inst->array_size_expression;
            
            typecheck_expression(size_expr);
            if (compiler->errors_reported) return nullptr;
            
            if (get_type_info(size_expr)->type != Ast_Type_Info::INTEGER) {
                compiler->report_error(size_expr, "Array-type size specifier must be an integer.\n");
                return nullptr;
            }
            
            auto lit = resolves_to_literal_value(size_expr);
            
            if (!lit) {
                compiler->report_error(type_inst, "Array-type size specifier must resolve to a literal expression.\n");
                return nullptr;
            }
            
            auto array_type = make_array_type(element, lit->integer_value, false);
            type_inst->type_value = array_type;
            return type_inst->type_value;
        }
        
        auto array_type = make_array_type(element, -1, type_inst->array_is_dynamic);
        type_inst->type_value = array_type;
        return type_inst->type_value;
    }
    
    assert(false);
    return nullptr;
}

void Sema::typecheck_function(Ast_Function *function) {
    if (function->type_info) return;
    
    for (auto &a : function->arguments) {
        a->is_readonly_variable = true;
        typecheck_expression(a);
    }
    
    if (function->return_decl) typecheck_expression(function->return_decl);
    
    if (compiler->errors_reported) return;
    
    if (function->is_c_varargs && !function->is_c_function) {
        compiler->report_error(function, "Function must be tagged @c_function in order to use temporary_c_vararg.\n");
    }
    
    if (function->is_c_function) {
        function->linkage_name = function->identifier->name->name;
    } else {
        function->linkage_name = get_mangled_name(compiler, function);
        String name = function->linkage_name;
        printf("Mangled name: '%.*s'\n", name.length, name.data);
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
    
    // @Incomplete I'm setting this as void for now just so we dont end up typechecking the same function multiple times, but this should be of 'function' type.
    function->type_info = compiler->type_void;
    
    if (function->scope) {
        typecheck_scope(function->scope);
    }
}
