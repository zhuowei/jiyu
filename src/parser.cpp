
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
    return parse_identifier();
}

Ast_Expression *Parser::parse_postfix_expression() {
    return parse_primary_expression();
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
        return parse_variable_declaration(true);
    }


    Ast_Expression *left = parse_expression();
    if (!left) return nullptr;

    token = peek_token();
    if (token->type == Token::EQUALS) {
        Ast_Expression *right = parse_expression();
        Ast_Binary_Expression *bin = new Ast_Binary_Expression();
        bin->operator_type = Token::EQUALS;
        bin->left = left;
        bin->right = right;
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
        if (stmt) scope->statements.add(stmt);

        if (compiler->errors_reported) return;

        token = peek_token();
    }

    if (requires_braces && !expect_and_eat((Token::Type) '}')) return;
}

Ast_Declaration *Parser::parse_variable_declaration(bool expect_var_keyword) {
    if (expect_var_keyword && !expect_and_eat(Token::KEYWORD_VAR)) return nullptr;

    Ast_Identifier *ident = parse_identifier();
    if (!ident) return nullptr;

    if (!expect_and_eat(Token::COLON)) return nullptr;

    Ast_Type_Info *type_info = parse_type_info();
    if (!type_info) return nullptr;

    Ast_Declaration *decl = new Ast_Declaration();
    decl->identifier = ident;
    decl->type_info = type_info;

    return decl;
}

Ast_Type_Info *Parser::parse_type_info() {
    Token *token = peek_token();

    if (token->type == Token::KEYWORD_INT) {
        next_token();
        return compiler->type_int32; // @IntegerSize ??
    }

    if (token->type == Token::KEYWORD_VOID) {
        next_token();
        return compiler->type_void;
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


    if (token->type == '{') {
        Ast_Scope *scope = new Ast_Scope();
        parse_scope(scope, true);

        function->scope = scope;
    }

    function->identifier = ident;
    return function;
}
