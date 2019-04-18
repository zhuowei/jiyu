
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
    Ast_Expression *parse_primary_expression();
    Ast_Expression *parse_postfix_expression();
    Ast_Expression *parse_multiplicative_expression();
    Ast_Expression *parse_additive_expression();
    Ast_Expression *parse_expression();
    Ast_Expression *parse_statement();

    Ast_Type_Info *parse_type_info();

    Ast_Declaration *parse_variable_declaration(bool expect_var_keyword);
    void parse_scope(Ast_Scope *scope, bool requires_braces);
    Ast_Function *parse_function();
};

#endif
