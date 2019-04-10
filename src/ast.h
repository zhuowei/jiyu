
#ifndef AST_H
#define AST_H

#include "general.h"

struct Atom;

struct Ast {
    Span span;
};

struct Ast_Expression {

};

struct Ast_Statement {

};

struct Ast_Identifier : Ast_Expression {
    Atom *name = nullptr;
};

struct Ast_Declaration : Ast_Statement {
    Ast_Identifier *identifier = nullptr;
    Ast_Expression *initializer_expression = nullptr;
};

struct Ast_Scope : Ast_Statement {
    Ast_Scope *parent = nullptr;
    Array<Ast_Statement *> statements;
};

struct Ast_Function : Ast_Statement {
    Ast_Identifier *identifier;

    Array<Ast_Declaration *> arguments;
    Array<Ast_Declaration *> returns;

    Ast_Scope *scope = nullptr;
};


#endif
