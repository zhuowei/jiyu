
#ifndef COMPILER_H
#define COMPILER_H

#include "general.h"
#include "ast.h"

#include <stdarg.h>

struct Lexer;
struct Parser;
struct Token;
struct Span;
struct LLVM_Generator;
struct Sema;

struct Atom {
    String name;
    u32 hash;
    
};

struct Atom_Table {
    Array<Atom *> data;
    
    
    Atom *find_atom(String name) {
        u32 hash = hash_key(name);
        for (array_count_type i = 0; i < data.count; ++i) {
            auto it = data[i];
            if (it->hash == hash) {
                if (it->name == name) return it;
            }
        }
        
        return nullptr;
    }
    
    u32 hash_key(String str) {
        u32 hash = 5381;
        s16 c = 0;;
        
        for (string_length_type i = 0; i < str.length; ++i) {
            // double cast to ensure sign extension
            c =  (s16) (s8) str[i];
            hash = ((hash << 5) + hash) + (u32) c;
        }
        
        return hash;
    }
};

struct Compiler {
    s64 errors_reported = 0;
    
    Parser *parser;
    Sema *sema;
    LLVM_Generator *llvm_gen;
    
    Atom_Table *atom_table;
    
    Ast_Scope *global_scope;
    
    Ast_Type_Info *type_void;
    Ast_Type_Info *type_bool;
    Ast_Type_Info *type_int8;
    Ast_Type_Info *type_int16;
    Ast_Type_Info *type_int32;
    Ast_Type_Info *type_int64;
    
    Ast_Type_Info *type_uint8;
    Ast_Type_Info *type_uint16;
    Ast_Type_Info *type_uint32;
    Ast_Type_Info *type_uint64;
    
    Ast_Type_Info *type_float32;
    Ast_Type_Info *type_float64;
    
    Ast_Type_Info *type_string;
    Ast_Type_Info *type_string_data;
    Ast_Type_Info *type_string_length;
    
    
    Compiler() {
        atom_table = new Atom_Table();
        global_scope = new Ast_Scope();
    }
    
    char *get_temp_c_string(String s);
    
    void init();
    
    Atom *make_atom(String name);
    
    void report_error_valist(String filename, String source, Span error_location, char *fmt, va_list args);
    void report_error(Token *tok, char *fmt, ...);
    void report_error(Ast *ast, char *fmt, ...);
};

Ast_Type_Info *make_pointer_type(Ast_Type_Info *pointee);

bool types_match(Ast_Type_Info *left, Ast_Type_Info *right);

inline
bool is_int_type(Ast_Type_Info *info) {
    return info->type == Ast_Type_Info::INTEGER;
}

inline
bool is_float_type(Ast_Type_Info *info) {
    return info->type == Ast_Type_Info::FLOAT;
}

inline
bool is_pointer_type(Ast_Type_Info *info) {
    return info->type == Ast_Type_Info::POINTER;
}

inline
Ast_Type_Info *get_type_info(Ast_Expression *expr) {
    while (expr->substitution) expr = expr->substitution;
    
    return expr->type_info;
}

inline
bool is_valid_primitive_cast(Ast_Type_Info *target, Ast_Type_Info *source) {
    if (target->type == Ast_Type_Info::POINTER) {
        return (source->type == Ast_Type_Info::INTEGER || source->type == Ast_Type_Info::POINTER);
    }
    
    if (target->type == Ast_Type_Info::INTEGER) {
        return (source->type == Ast_Type_Info::INTEGER || source->type == Ast_Type_Info::POINTER || source->type == Ast_Type_Info::FLOAT);
    }
    
    if (target->type == Ast_Type_Info::FLOAT) {
        return (source->type == Ast_Type_Info::FLOAT || source->type == Ast_Type_Info::INTEGER);
    }
    
    return false;
}

#endif
