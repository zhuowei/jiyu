
#include "compiler.h"
#include "lexer.h"

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