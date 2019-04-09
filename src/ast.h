
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

struct Ast_Scope : Ast_Statement {
    Array<Ast_Statement *> statements;
};

struct Ast_Identifier {
    Atom *name;
};

struct Ast_Lambda {

};

struct Ast_Function : Ast_Statement {

    Ast_Lambda *lambda;
};


#endif
