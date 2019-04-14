
#include "lexer.h"
#include "compiler.h"


static bool is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\v' || c == '\r' || c == '\n';
}

static char tolower(char c) {
    if (c >= 'A' && c <= 'Z') return c + 0x20;

    return c;
}

static bool is_letter(char c) {
    c = tolower(c);
    return c >= 'a' && c <= 'z';
}

static bool is_digit(char c) {
    return c >= '0' && c <= '9';
}

// @TODO UTF8 identifier support?
static bool starts_identifier(char c) {
    return c == '_' || is_letter(c); 
}

static bool continues_identifier(char c) {
    return starts_identifier(c) || is_digit(c);
}

Token Lexer::make_token(Token::Type type, Span span) {
    Token t = Token(type, TextSpan(text, span));
    t.filename = filename;
    return t;
}

Token Lexer::make_eof_token() {
    return make_token(Token::END, Span(text.length, 0));
}

Token Lexer::make_string_token(Token::Type type, Span span, String string) {
    Token t = make_token(type, span);
    t.string = string;
    return t;
}

Token Lexer::make_integer_token(s64 value, Span span) {
    Token t = make_token(Token::INTEGER, span);
    t.integer = value;
    return t;
}

void Lexer::eat_whitespace() {
    while (current_char < text.length && is_whitespace(text[current_char])) {
        current_char++;
    }
}

Token Lexer::lex_token() {
    eat_whitespace();

    if (!(current_char < text.length)) return make_eof_token();

    if (starts_identifier(text[current_char])) {
        auto start = current_char;
        while (current_char < text.length && continues_identifier(text[current_char])) {
            current_char++;
        }

        string_length_type length = current_char - start;
        Token result = make_string_token(Token::IDENTIFIER, Span(start, length), text.substring(start, length));

        // @Cleanup find a faster way to implement these things
        if      (result.string == to_string("func")) result.type = Token::KEYWORD_FUNC;
        else if (result.string == to_string("var"))  result.type = Token::KEYWORD_VAR;
        else if (result.string == to_string("void")) result.type = Token::KEYWORD_VOID;
        else if (result.string == to_string("int"))  result.type = Token::KEYWORD_INT;
        return result;
    } else if (is_digit(text[current_char])) {
        auto start = current_char;

        while (current_char < text.length && is_digit(text[current_char])) current_char++;

        char *value_string = compiler->get_temp_c_string(text.substring(start, current_char - start));
        auto value = atoll(value_string);

        return make_integer_token(value, Span(start, current_char - start));
    } else if (text[current_char] == '-') {
        if (current_char+1 < text.length && text[current_char+1] == '>') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::ARROW, Span(start, 2));
        }
    } else if (text[current_char] == '/') {
        if (current_char+1 < text.length && text[current_char+1] == '*') {
            string_length_type start = current_char;
            current_char += 2;
            s64 stack = 1;
            while (current_char < text.length && stack > 0) {
                if (text[current_char] == '*') {
                    if (current_char+1 < text.length && text[current_char+1] == '/') {
                        stack--;
                        current_char += 2;
                        continue;
                    }
                }

                if (text[current_char] == '/') {
                    if (current_char+1 < text.length && text[current_char+1] == '*') {
                        stack++;
                        current_char += 2;
                        continue;
                    }
                }

                current_char++;
            }

            string_length_type length = current_char - start;
            // :CommentTokens:
            // Returning a token here because we dont return pointers to tokens
            // and if we returned a recursive lex_token() call, we can get defeated quite quickly from
            // people spamming comment-after-comment.

            Token result = make_string_token(Token::COMMENT, Span(start, length), text.substring(start, length));
            return result;
        } else if (current_char+1 < text.length && text[current_char+1] == '/') {
            string_length_type start = current_char;
            current_char += 2;

            while (current_char < text.length && text[current_char] != '\n') current_char++;

            string_length_type length = current_char - start;
            
            // :CommentTokens:
            Token result = make_string_token(Token::COMMENT, Span(start, length), text.substring(start, length));
            return result;
        }
    }

    char c = text[current_char];
    auto start = current_char++;
    return make_token((Token::Type)c, Span(start, 1));
}

void Lexer::tokenize_text() {
    Token tok;
    do {
        tok = lex_token();

        // Ignore Token::COMMENT since in most cases we dont care about these
        if (tok.type == Token::COMMENT) continue;

        tokens.add(tok);
    } while (tok.type != Token::END);
}
