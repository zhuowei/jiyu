
#ifndef AST_H
#define AST_H

#include "general.h"
#include "lexer.h"

struct Atom;
struct Ast_Declaration;

enum Ast_Type {
    AST_UNINITIALIZED,
    AST_BINARY_EXPRESSION,
    AST_UNARY_EXPRESSION,
    AST_IDENTIFIER,
    AST_DECLARATION,
    AST_SCOPE,
    AST_FUNCTION,
    AST_LITERAL,
    AST_FUNCTION_CALL,
    AST_DEREFERENCE,
    AST_CAST,
    AST_IF,
    AST_WHILE,
    AST_RETURN,
    AST_TYPE_INSTANTIATION,
    AST_TYPE_ALIAS,
};

struct Ast {
    TextSpan text_span;
    String filename;
    
    Ast_Type type;
};

struct Ast_Type_Info {
    enum Type {
        UNINITIALIZED = 0,
        
        // primitves
        VOID,
        BOOL,
        INTEGER,
        FLOAT,
        STRING,
        
        // non-primitves
        POINTER,
        ALIAS,
        FUNCTION,
        TYPE, // meta type assigned to typealias and struct
        
        // User defined (mostly)
        STRUCT,
    };
    
    Type type = UNINITIALIZED;
    
    
    bool is_signed = false; // for INTEGER
    
    Ast_Type_Info *pointer_to = nullptr;
    Ast_Type_Info *alias_of   = nullptr;
    
    // Ast_Function *function;
    
    s64 size = -1;
};

struct Ast_Expression : Ast {
    Ast_Type_Info *type_info = nullptr;
    
    Ast_Expression *substitution = nullptr;
};

struct Ast_Type_Info;
struct Ast_Identifier;

struct Ast_Type_Instantiation : Ast_Expression {
    Ast_Type_Instantiation() { type = AST_TYPE_INSTANTIATION; }
    
    Ast_Type_Info *builtin_primitive = nullptr;
    Ast_Type_Instantiation *pointer_to = nullptr;
    Ast_Identifier *typename_identifier = nullptr;
};

struct Ast_Type_Alias : Ast_Expression {
    Ast_Type_Alias() { type = AST_TYPE_ALIAS; }
    
    Ast_Identifier *identifier = nullptr;
    Ast_Type_Instantiation *internal_type_inst = nullptr;
};

struct Ast_Unary_Expression : Ast_Expression {
    Ast_Unary_Expression() { type = AST_UNARY_EXPRESSION; }
    
    Token::Type operator_type;
    Ast_Expression *expression = nullptr;
};

struct Ast_Binary_Expression : Ast_Expression {
    Ast_Binary_Expression() { type = AST_BINARY_EXPRESSION; }
    
    Token::Type operator_type;
    Ast_Expression *left  = nullptr;
    Ast_Expression *right = nullptr;
};

struct Ast_Identifier : Ast_Expression {
    Ast_Identifier() { type = AST_IDENTIFIER; }
    Atom *name = nullptr;
    
    Ast_Expression *resolved_declaration = nullptr;
};

struct Ast_Dereference : Ast_Expression {
    Ast_Dereference() { type = AST_DEREFERENCE; }
    
    Ast_Expression *left;
    Ast_Identifier *field_selector = nullptr;
    
    s64 element_path_index = -1; // 0-based element into the list of declarations or fields within the outer type
    s64 byte_offset = -1; // byte-offset from the start of the the memory occupied by the outer type
};

struct Ast_Function_Call : Ast_Expression {
    Ast_Function_Call() { type = AST_FUNCTION_CALL; }
    
    // @Incomplete this should be an expression instead of an identifier.
    Ast_Identifier *identifier = nullptr;
    Array<Ast_Expression *> argument_list;
};

struct Ast_If : Ast_Expression {
    Ast_If() { type = AST_IF; }
    
    Ast_Expression *condition = nullptr;
    
    Ast_Expression *then_statement = nullptr;
    Ast_Expression *else_statement = nullptr;
};

struct Ast_While : Ast_Expression {
    Ast_While() { type = AST_WHILE; }
    
    Ast_Expression *condition = nullptr;
    Ast_Expression *statement = nullptr;
};

struct Ast_Return : Ast_Expression {
    Ast_Return() { type = AST_RETURN; }
    
    Ast_Expression *expression = nullptr;
};

struct Ast_Literal : Ast_Expression {
    Ast_Literal() { type = AST_LITERAL; }
    
    enum Type {
        INTEGER,
        STRING,
        FLOAT,
        BOOL,
    };
    
    Type literal_type;
    
    bool bool_value;
    s64 integer_value;
    double float_value;
    String string_value;
};

struct Ast_Declaration : Ast_Expression {
    Ast_Declaration() { type = AST_DECLARATION; }
    Ast_Identifier *identifier = nullptr;
    Ast_Expression *initializer_expression = nullptr;
    
    Ast_Type_Instantiation *type_inst = nullptr;
    
    bool is_let = false;
    bool is_function_argument = false;
};

struct Ast_Function;

struct Ast_Scope : Ast_Expression {
    Ast_Scope() { type = AST_SCOPE; }
    Ast_Scope *parent = nullptr;
    Array<Ast_Expression *> statements;
    Array<Ast_Expression *> declarations; // really should only contain Ast_Declaration and Ast_Function
    
    Ast_Function   *owning_function = nullptr;
    Ast_Expression *owning_statement = nullptr;
};

struct Ast_Function : Ast_Expression {
    Ast_Function() { type = AST_FUNCTION; }
    
    Ast_Identifier *identifier;
    
    Array<Ast_Declaration *> arguments;
    Ast_Declaration *return_decl = nullptr;
    
    Ast_Scope *scope = nullptr;
    
    bool is_c_function = false;
    bool is_c_varargs = false;
};

struct Ast_Cast : Ast_Expression {
    Ast_Cast() { type = AST_CAST; }
    Ast_Type_Instantiation *target_type_inst = nullptr;
    Ast_Expression *expression = nullptr;
};

#define AST_NEW(type) (type *)ast_init(this, new type());

#endif
