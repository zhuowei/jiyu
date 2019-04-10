
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

void Parser::parse_scope(Ast_Scope *scope, bool requires_braces) {
    if (requires_braces && !expect_and_eat((Token::Type) '{')) return;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (requires_braces && token->type == '}') break;

        Ast_Statement *stmt = parse_function();
        if (stmt) scope->statements.add(stmt);

        if (compiler->errors_reported) return;

        token = peek_token();
    }

    if (requires_braces && !expect_and_eat((Token::Type) '}')) return;
}

Ast_Declaration *Parser::parse_variable_declaration(bool expect_var_keyword) {
    if (expect_var_keyword && !expect_and_eat(Token::KEYWORD_VAR)) return nullptr;

    // @TODO
}

Ast_Function *Parser::parse_function() {
    expect_and_eat(Token::KEYWORD_FUNC);

    Ast_Function *function = new Ast_Function();

    Ast_Identifier *ident = parse_identifier();
    if (!ident) return nullptr;

    if (!expect_and_eat((Token::Type) '(')) return nullptr;
    
    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == ')') break;

        Ast_Declaration *decl = parse_variable_declaration(false);
        if (decl) function->arguments.add(decl);

        if (compiler->errors_reported) return nullptr;

        token = peek_token();
    }

    if (!expect_and_eat((Token::Type) ')')) return nullptr;

    if (!expect_and_eat(Token::ARROW)) return nullptr;

    // @Temporary
    if (!expect_and_eat(Token::KEYWORD_INT)) return nullptr;

    Ast_Scope *scope = new Ast_Scope();
    parse_scope(scope, true);

    function->identifier = ident;
    function->scope = scope;

    return function;
}
