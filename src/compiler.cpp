
#include "general.h"
#include "compiler.h"
#include "lexer.h"
#include "parser.h"

#include <stdio.h> // for vprintf


bool types_match(Ast_Type_Info *left, Ast_Type_Info *right) {
    if (left->type != right->type) return false;
    if (left->size != right->size) return false;
    
    if (left->type == Ast_Type_Info::POINTER) {
        assert(left->pointer_to && right->pointer_to);
        return types_match(left->pointer_to, right->pointer_to);
    }
    
    if (left->type == Ast_Type_Info::ARRAY) {
        return types_match(left->array_element, right->array_element) &&
            left->array_element_count == right->array_element_count &&
            left->is_dynamic == right->is_dynamic;
    }
    
    if (left->type == Ast_Type_Info::STRUCT) {
        // @Incomplete how would this work for anonymous structs for which a struct declaration doesnt exist? Do we always just create a faux declaration in that case?
        assert(left->struct_decl);
        assert(right->struct_decl);
        return left->struct_decl == right->struct_decl;
    }
    
    return true;
}

Ast_Type_Info *make_array_type(Ast_Type_Info *element, array_count_type count, bool is_dynamic) {
    Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::ARRAY;
    info->array_element       = element;
    info->array_element_count = count;
    info->is_dynamic = is_dynamic;
    
    if (count >= 0) {
        assert(is_dynamic == false);
        info->size = element->size * count;
        info->alignment = element->alignment;
    } else {
        if (!is_dynamic) {
            info->size = 16; // @Cleanup hardcoded value
        } else {
            info->size = 24; // @Cleanup hardcoded value
        }
        
        info->alignment = 8; // @TargetInfo @PointerSize @Cleanup hardcoded value
    }
    return info;
}

Ast_Type_Info *make_pointer_type(Ast_Type_Info *pointee) {
    Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::POINTER;
    info->pointer_to = pointee;
    info->size = 8; // @TargetInfo
    info->alignment = info->size;
    return info;
}

s64 pad_to_alignment(s64 current, s64 align) {
    assert(align >= 1);
    
    s64 minum = current & (align-1);
    if (minum) {
        current += align - minum;
    }
    
    return current;
}

Ast_Type_Info *make_struct_type(Ast_Struct *_struct) {
    Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::STRUCT;
    
    s64 size_cursor = 0;
    s64 biggest_alignment = 1;
    
    for (auto expr : _struct->member_scope.declarations) {
        assert(expr->type == AST_DECLARATION);
        
        // @Cleanup @Hack we need to be able to handle other structs, functions, typealiases or at least punt on them.
        auto decl = static_cast<Ast_Declaration *>(expr);
        assert(decl && decl->type_info);
        
        Ast_Type_Info::Struct_Member member;
        member.name = decl->identifier->name;
        member.type_info = decl->type_info;
        member.is_let = decl->is_let;
        
        info->struct_members.add(member);
        
        size_cursor = pad_to_alignment(size_cursor, member.type_info->alignment);
        size_cursor += member.type_info->size;
        
        if (member.type_info->alignment > biggest_alignment) {
            biggest_alignment = member.type_info->alignment;
        }
    }
    
    info->alignment = biggest_alignment;
    info->struct_decl = _struct;
    info->size = size_cursor;
    info->stride = pad_to_alignment(info->size, info->alignment);
    return info;
}

static Ast_Type_Info *make_int_type(bool is_signed, s64 size) {
    Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::INTEGER;
    info->is_signed = is_signed;
    info->size = size;
    info->alignment = info->size;
    return info;
}

static Ast_Type_Info *make_float_type(s64 size) {
    Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::FLOAT;
    info->size = size;
    info->alignment = info->size;
    return info;
}

char *Compiler::get_temp_c_string(String s) {
    char *mem = (char *)malloc(s.length + 1); // @Leak
    
    memcpy(mem, s.data, s.length);
    mem[s.length] = 0;
    return mem;
}

void Compiler::init() {
    type_void = new Ast_Type_Info();
    type_void->type = Ast_Type_Info::VOID;
    
    type_bool = new Ast_Type_Info();
    type_bool->type = Ast_Type_Info::BOOL;
    
    type_int8  = make_int_type(true, 1);
    type_int16 = make_int_type(true, 2);
    type_int32 = make_int_type(true, 4);
    type_int64 = make_int_type(true, 8);
    
    type_uint8  = make_int_type(false, 1);
    type_uint16 = make_int_type(false, 2);
    type_uint32 = make_int_type(false, 4);
    type_uint64 = make_int_type(false, 8);
    
    type_float32 = make_float_type(4);
    type_float64 = make_float_type(8);
    
    type_string_data = make_pointer_type(type_uint8);
    // @FixMe
    type_string_length = type_int64; // @TargetInfo

    type_string = new Ast_Type_Info();
    type_string->type = Ast_Type_Info::STRING;
    type_string->size = type_string_length->size + type_string_data->size;
    
    type_array_count   = type_int64; // @TargetInfo
    
    type_info_type = new Ast_Type_Info();
    type_info_type->type = Ast_Type_Info::TYPE;
    
    type_ptr_void = make_pointer_type(type_void);
    
    atom_data      = make_atom(to_string("data"));
    atom_length    = make_atom(to_string("length"));
    atom_count     = make_atom(to_string("count"));
    atom_allocated = make_atom(to_string("allocated"));
    atom_it        = make_atom(to_string("it"));
    atom_it_index  = make_atom(to_string("it_index"));
    atom_main      = make_atom(to_string("main"));
}

Atom *Compiler::make_atom(String name) {
    Atom *atom = atom_table->find_atom(name);
    if (!atom) {
        atom = new Atom();
        atom->name = copy_string(name);
        atom->hash = atom_table->hash_key(name);
        
        atom_table->data.add(atom);
    }
    
    return atom;
}

#define TTY_RED    "\033[0;31m"
#define TTY_RESET  "\033[0m"

void Compiler::report_error_valist(String filename, String source, Span error_location, char *fmt, va_list args) {
    
    string_length_type l0;
    string_length_type c0;
    
    string_length_type l1;
    string_length_type c1;
    
    error_location.map_to_text_coordinates(source, &l0, &c0, &l1, &c1);
    
    printf("%.*s:%d,%d: ", filename.length, filename.data, l0, c0);
    vprintf(fmt, args);
    printf("\n");
    
    string_length_type start_char = -1;
    string_length_type end_char   = -1;
    string_length_type num_lines  = -1;
    error_location.get_surrounding_lines(source, 1, &start_char, &end_char, &num_lines);
    
    assert(start_char >= 0 && end_char >= 0);
    assert(num_lines >= 0);
    
    // printf("start char: %d\n", start_char);
    // printf("end   char: %d\n", end_char);
    // printf("Span: %d, %d\n", error_location.start, error_location.start + error_location.length - 1);
    
    String s;
    s.data = source.data + start_char;
    s.length = end_char - start_char;
    
    string_length_type char_current = start_char;
    for (string_length_type i = 0; i < num_lines; ++i) {
        String temp = s;
        
        printf(">    ");
        
        while (s.length > 0 && s[0] != '\n') {
            // printf("char_current: %d\n", char_current);
            if (char_current == error_location.start) {
                printf(TTY_RED);
            } else if (char_current == (error_location.start + error_location.length)) {
                printf(TTY_RESET);
            }
            
            putchar(s[0]);
            advance(&s);
            char_current++;
        }
        
        char_current++; // count newline character
        if (char_current == (error_location.start + error_location.length)) {
            printf(TTY_RESET);
        }
        
        temp.length = temp.length - s.length;
        
        s.data = temp.data + temp.length + 1;
        s.length = (end_char - start_char) - (temp.length + 1);
        
        putchar('\n');
    }
    
    putchar('\n');
    
    errors_reported += 1;
}

void Compiler::report_error(Token *tok, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    String filename;
    String source;
    Span span;
    
    if (tok) {
        filename = tok->filename;
        source = tok->text_span.string;
        span = tok->text_span.span;
    }
    
    report_error_valist(filename, source, span, fmt, args);
    va_end(args);
    
    // __builtin_debugtrap();
}


void Compiler::report_error(Ast *ast, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    
    String filename;
    String source;
    Span span;
    
    if (ast) {
        filename = ast->filename;
        source = ast->text_span.string;
        span = ast->text_span.span;
    }
    
    report_error_valist(filename, source, span, fmt, args);
    va_end(args);
}
