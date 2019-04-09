
#ifndef LEXER_H
#define LEXER_H

#include "general.h"

struct Token {
    enum Type {
        END = 256,
        IDENTIFIER,
        KEYWORD_FUNC,
        KEYWORD_VOID,
        KEYWORD_INT,

        ARROW,

        COMMENT,
    };

    Type type;
    TextSpan text_span;
    String filename;

    String string;

    Token() {}

    Token(Type type, TextSpan span) {
        this->type = type;
        this->text_span = span;
    }
};

struct Compiler;

struct Lexer {
    String filename;

    s64 current_char = 0;
    String text;

    Array<Token> tokens;
    Compiler *compiler;

    Lexer(Compiler *compiler, String input_text, String filename) {
        this->text = input_text;
        this->filename = filename;
        this->compiler = compiler;
    }

    Token make_token(Token::Type type, Span span);
    Token make_eof_token();
    Token make_string_token(Token::Type type, Span span, String string);
    
    void eat_whitespace();
    Token lex_token();
    void tokenize_text();
};

#endif
