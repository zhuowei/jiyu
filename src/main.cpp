
#include "general.h"

#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include "llvm.h"


#include <stdio.h>

bool read_entire_file(String filepath, String *result) {
    char *cpath = to_c_string(filepath);

    FILE *file = fopen(cpath, "rb");
    if (!file) {
        free(cpath);
        return false;
    }

    fseek(file, 0, SEEK_END);
    auto size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *mem = (char *)malloc(size);
    auto bytes_read = fread(mem, 1, size, file);
    if (bytes_read != (size_t)size) {
        fclose(file);
        free(mem);
        free(cpath);
        return false;
    }

    String s;
    s.data = mem;
    s.length = size;
    *result = s;
    free(cpath);
    return true;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("ERROR: no input files\n");
        return -1;
    }

    String filename = to_string(argv[1]);
    String source;
    bool success = read_entire_file(filename, &source);
    if (!success) {
        printf("Could not open file: %.*s\n", (int)filename.length, filename.data);
        return -1;
    }

    Compiler compiler;
    compiler.init();
    Lexer *lexer = new Lexer(&compiler, source, filename);
    lexer->tokenize_text();

    Parser *parser = new Parser(lexer);
    compiler.parser = parser;

    printf("File contents: %.*s\n", source.length, source.data);

    printf("Tokens:\n");

    printf("TOKEN COUNT: %d\n", lexer->tokens.count);
    for (auto &t : lexer->tokens) {
        String text = t.text_span.get_text();
        printf("'%.*s'\n", text.length, text.data);
    }

    compiler.parser->parse_scope(compiler.global_scope, false);
    compiler.llvm_gen = new LLVM_Generator(&compiler);
    compiler.llvm_gen->init();

    for (auto &stmt : compiler.global_scope->statements) {
        if (stmt->type == AST_FUNCTION) {
            auto function = reinterpret_cast<Ast_Function *>(stmt);
            compiler.llvm_gen->emit_function(function);
        }
    }
}
