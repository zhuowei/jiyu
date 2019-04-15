
#include "parser.h"
#include "compiler.h"

Token *Parser::next_token() {
    return &lexer->tokens[current_token++];
}

Token *Parser::peek_token() {
    return &lexer->tokens[current_token];
}

bool Parser::expect(Token::Type type) {
    Token *token = peek_token();

    if (token->type != type) {
        compiler->report_error(token, "Expected token of type '%d' but got '%d'.\n", type, token->type);
        return false;
    }

    return true;
}

bool Parser::expect_and_eat(Token::Type type) {
    Token *token = next_token();

    if (token->type != type) {
        compiler->report_error(token, "Expected token of type '%d' but got '%d'.\n", type, token->type);
        return false;
    }

    return true;
}

Ast_Identifier *Parser::parse_identifier() {
    if (!expect(Token::IDENTIFIER)) return nullptr;

    Token *token = next_token();
    String name = token->string;

    Atom *atom = compiler->make_atom(name);
    assert(atom);

    Ast_Identifier *ident = new Ast_Identifier();
    ident->name = atom;
    return ident;
}

Ast_Expression *Parser::parse_primary_expression() {
    Token *token = peek_token();

    if (token->type == Token::IDENTIFIER) {
        auto ident = parse_identifier();
        return ident;
    }

    if (token->type == Token::INTEGER) {
        next_token();

        Ast_Literal *lit = new Ast_Literal();
        lit->literal_type = Ast_Literal::INTEGER;
        lit->integer_value = token->integer;

        // @TODO mark for infer

        return lit;
    }

    if (token->type == Token::STRING) {
        next_token();

        Ast_Literal *lit = new Ast_Literal();
        lit->literal_type = Ast_Literal::STRING;
        lit->string_value = token->string;

        // @TODO mark for infer

        return lit;
    }

    return nullptr;
}

Ast_Expression *Parser::parse_postfix_expression() {
    Ast_Expression *sub_expression = parse_primary_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::LEFT_PAREN) {
            next_token();

            // transform this into a function call
            Ast_Function_Call *call = new Ast_Function_Call();

            assert(sub_expression->type == AST_IDENTIFIER);
            // @HACK @HACK @HACK
            // @HACK @HACK @HACK
            // @HACK @HACK @HACK
            // @HACK @HACK @HACK
            call->identifier = reinterpret_cast<Ast_Identifier *>(sub_expression);

            bool found_argument = false;
            token = peek_token();
            while (token->type != Token::END) {

                if (call->argument_list.count > 0 && token->type == ',') {
                    next_token();
                } else if (token->type == ')') break;

                auto expr = parse_expression();
                if (!expr) {
                    // @FixME report_error
                    compiler->report_error(nullptr, "Malformed expression found while parsing parameter list.\n");
                    return nullptr;
                }
                call->argument_list.add(expr);

                token = peek_token(); 
            }

            if (!expect_and_eat((Token::Type) ')')) return nullptr;

            sub_expression = call;
        } else if (token->type == Token::DOT) {
            next_token();

            Ast_Dereference *deref = new Ast_Dereference();

            // @TODO do other languages let you use anything other than an identifier for a field selection?
            auto right = parse_identifier();
            if (!right) return nullptr;

            deref->left = sub_expression;
            deref->field_selector = right;

            sub_expression = deref;
        } else {
            break;
        }


        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_expression() {
    return parse_postfix_expression();
}

Ast_Expression *Parser::parse_statement() {
    Token *token = peek_token();

    if (token->type == Token::KEYWORD_FUNC) {
        return parse_function();
    }

    if (token->type == Token::KEYWORD_VAR) {
        auto var = parse_variable_declaration(true);
        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;
        return var;
    }


    Ast_Expression *left = parse_expression();
    if (!left) return nullptr;

    token = peek_token();
    if (token->type == Token::EQUALS) {
        next_token();

        Ast_Expression *right = parse_expression();
        Ast_Binary_Expression *bin = new Ast_Binary_Expression();
        bin->operator_type = Token::EQUALS;
        bin->left = left;
        bin->right = right;

        left = bin;
    }

    if (!expect_and_eat(Token::SEMICOLON)) return nullptr;

    return left;
}

void Parser::parse_scope(Ast_Scope *scope, bool requires_braces) {
    if (requires_braces && !expect_and_eat((Token::Type) '{')) return;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (requires_braces && token->type == '}') break;

        Ast_Expression *stmt = parse_statement();
        if (stmt) {
            scope->statements.add(stmt);

            if (stmt->type == AST_DECLARATION || stmt->type == AST_FUNCTION) {
                scope->declarations.add(stmt);
            }
        }

        if (compiler->errors_reported) return;

        token = peek_token();
    }

    if (requires_braces && !expect_and_eat((Token::Type) '}')) return;
}

Ast_Declaration *Parser::parse_variable_declaration(bool expect_var_keyword) {
    if (expect_var_keyword && !expect_and_eat(Token::KEYWORD_VAR)) return nullptr;

    Token *ident_token = peek_token(); // used for error below, @Cleanup we want to be able to report errors using an Ast
    Ast_Identifier *ident = parse_identifier();
    if (!ident) return nullptr;

    Ast_Declaration *decl = new Ast_Declaration();
    decl->identifier = ident;

    Token *token = peek_token();
    if (token->type == Token::COLON) {
        next_token();

        Ast_Type_Info *type_info = parse_type_info();
        if (!type_info) return nullptr;

        
        decl->type_info = type_info;
    }

    token = peek_token();
    if (token->type == Token::EQUALS) {
        next_token();

        Ast_Expression *expression = parse_expression();
        if (!expression) return nullptr;

        decl->initializer_expression = expression;
    }

    if (!decl->initializer_expression && !decl->type_info) {
        // @TODO maybe this should be moved to semantic analysis
        compiler->report_error(ident_token, "Declared variable must be declared with a type or be initialized.\n");
        return nullptr;
    }


    return decl;
}

Ast_Type_Info *Parser::parse_type_info() {
    Token *token = peek_token();

    if (token->type == Token::KEYWORD_INT) {
        next_token();
        return compiler->type_int32; // @IntegerSize ??
    }

    if (token->type == Token::KEYWORD_UINT8) {
        next_token();
        return compiler->type_uint8;
    }

    if (token->type == Token::KEYWORD_VOID) {
        next_token();
        return compiler->type_void;
    }

    if (token->type == Token::KEYWORD_STRING) {
        next_token();
        return compiler->type_string;
    }

    if (token->type == Token::STAR) {
        next_token();
        auto pointee = parse_type_info();
        if (!pointee) return nullptr;
        // @Incomplete report_error here?

        return make_pointer_type(pointee);
    }

    return nullptr;
}

Ast_Function *Parser::parse_function() {
    expect_and_eat(Token::KEYWORD_FUNC);

    Ast_Function *function = new Ast_Function();

    Ast_Identifier *ident = parse_identifier();
    if (!ident) return nullptr;

    if (!expect_and_eat((Token::Type) '(')) return nullptr;
    
    Token *token = peek_token();
    while (token->type != Token::END) {

        if (function->arguments.count > 0 && token->type == ',') {
            next_token();
        } else  if (token->type == ')') break;

        Ast_Declaration *decl = parse_variable_declaration(false);
        if (decl) function->arguments.add(decl);

        if (compiler->errors_reported) return nullptr;

        token = peek_token();
    }

    if (!expect_and_eat((Token::Type) ')')) return nullptr;

    if (peek_token()->type == Token::ARROW) {
        if (!expect_and_eat(Token::ARROW)) return nullptr;

        bool found_return_type = false;
        token = peek_token();
        while (token->type != Token::END) {

            if (token->type == '{' || token->type == Token::SEMICOLON) break;

            Ast_Identifier *ident = nullptr;
            if (token->type == Token::IDENTIFIER) {
                ident = parse_identifier();
                assert(ident);

                if (!expect_and_eat(Token::COLON)) return nullptr;
            }

            Ast_Type_Info *type_info = parse_type_info();
            if (!type_info) return nullptr;

            Ast_Declaration *decl = new Ast_Declaration();
            decl->identifier = ident;
            decl->type_info = type_info;

            function->returns.add(decl);

            found_return_type = true;

            token = peek_token();
        }

        if (!found_return_type) {
            compiler->report_error(token, "No return type found after arrow!\n");
            return nullptr;
        }
    }


    if (token->type == '{') {
        Ast_Scope *scope = new Ast_Scope();
        parse_scope(scope, true);

        function->scope = scope;
    } else {
        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;
    }

    function->identifier = ident;
    return function;
}
