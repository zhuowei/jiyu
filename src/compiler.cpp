
#include "general.h"
#include "compiler.h"
#include "lexer.h"
#include "parser.h"

#include <stdio.h> // for vprintf

Atom *Compiler::make_atom(String name) {
    Atom *atom = atom_table->find_atom(name);
    if (!atom) {
        atom = new Atom();
        atom->name = copy_string(name);
        atom->hash = atom_table->hash_key(name);
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
    report_error_valist(tok->filename, tok->text_span.string, tok->text_span.span, fmt, args);
    va_end(args);
}