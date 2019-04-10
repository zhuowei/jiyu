
#ifndef PARSER_H
#define PARSER_H

#include "general.h"
#include "ast.h"

#include "lexer.h" // for Lexer, Token, Token::Type

struct Compiler;

struct Parser {
    Compiler *compiler;
    Lexer *lexer;
    array_count_type current_token = 0;

    Parser(Lexer *lexer) {
        this->lexer = lexer;
        this->compiler = lexer->compiler;
    }

    Token *next_token();
    Token *peek_token();

    bool expect(Token::Type type);
    bool expect_and_eat(Token::Type type);

    Ast_Identifier *parse_identifier();
    void parse_scope(Ast_Scope *scope, bool requires_braces);
    Ast_Function *parse_function();
};

#endif
