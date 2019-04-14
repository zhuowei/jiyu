
#include "general.h"
#include "compiler.h"
#include "lexer.h"
#include "parser.h"

#include <stdio.h> // for vprintf

static Ast_Type_Info *make_int_type(bool is_signed, s64 size) {
    Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::INTEGER;
    info->is_signed = is_signed;
    info->size = size;
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

    type_int8  = make_int_type(true, 1);
    type_int16 = make_int_type(true, 2);
    type_int32 = make_int_type(true, 4);
    type_int64 = make_int_type(true, 8);

    type_uint8  = make_int_type(false, 1);
    type_uint16 = make_int_type(false, 2);
    type_uint32 = make_int_type(false, 4);
    type_uint64 = make_int_type(false, 8);
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

void Compiler::report_error_valist(String filename, String source, Span error_location, char *fmt, va_list args) {

    string_length_type l0;
    string_length_type c0;

    string_length_type l1;
    string_length_type c1;

    error_location.map_to_text_coordinates(parser->lexer->text, &l0, &c0, &l1, &c1);

    printf("%.*s:%d,%d: ", filename.length, filename.data, l0, c0);
    vprintf(fmt, args);

    // @TODO print the error region, highlight the span we care about

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