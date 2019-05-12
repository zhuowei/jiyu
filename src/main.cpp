
#include "general.h"

#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include "llvm.h"
#include "sema.h"

#include "microsoft_craziness.h"

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

u8 *get_command_line(Array<String> *strings) {
    string_length_type total_length = 0;
    
    for (String s : *strings) total_length += s.length;
    
    total_length += strings->count * 3 + 1; // enough space for quotes and a space around each argument
    
    string_length_type cursor = 0;
    u8 *final = reinterpret_cast<u8 *>(malloc(total_length));
    
    for (String s : *strings) {
        final[cursor++] = '\"';
        
        memcpy(final + cursor, s.data, s.length);
        cursor += s.length;
        
        final[cursor++] = '\"';
        final[cursor++] = ' ';
    }
    
    final[cursor++] = 0;
    return final;
}

#if WIN32

void run_command(Array<String> *args) {
    
}

#endif

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
    
    if (compiler.errors_reported) return -1;
    
    Parser *parser = new Parser(lexer);
    compiler.parser = parser;
    
    /*
    printf("File contents: %.*s\n", source.length, source.data);
    
    printf("Tokens:\n");
    
    printf("TOKEN COUNT: %d\n", lexer->tokens.count);
    for (auto &t : lexer->tokens) {
        String text = t.text_span.get_text();
        printf("'%.*s'\n", text.length, text.data);
    }
    */
    
    compiler.parser->parse_scope(compiler.global_scope, false);
    
    if (compiler.errors_reported) return -1;
    
    compiler.sema = new Sema(&compiler);
    compiler.sema->typecheck_scope(compiler.global_scope);
    
    if (compiler.errors_reported) return -1;
    
    compiler.llvm_gen = new LLVM_Generator(&compiler);
    compiler.llvm_gen->init();
    
    for (auto &stmt : compiler.global_scope->statements) {
        if (stmt->type == AST_FUNCTION) {
            auto function = reinterpret_cast<Ast_Function *>(stmt);
            compiler.llvm_gen->emit_function(function);
        }
    }
    
    if (compiler.errors_reported) return -1;
    
    compiler.llvm_gen->finalize();
    
    if (compiler.errors_reported) return -1;
    
#if WIN32
    auto win32_sdk = find_visual_studio_and_windows_sdk();
    
    if (win32_sdk.vs_exe_path) {
        const int LINE_SIZE = 4096;
        char exe_path[LINE_SIZE]; // @Cleanup hardcoded value
        char libpath[LINE_SIZE];
        
        Array<String> args;
        
        snprintf(exe_path, LINE_SIZE, "%S\\link.exe", win32_sdk.vs_exe_path);
        args.add(to_string(exe_path));
        
        if (win32_sdk.vs_library_path) {
            
            snprintf(libpath, LINE_SIZE, "/libpath:%S", win32_sdk.vs_library_path);
            args.add(copy_string(to_string(libpath)));
        }
        
        if (win32_sdk.windows_sdk_um_library_path) {
            snprintf(libpath, LINE_SIZE, "/libpath:%S", win32_sdk.windows_sdk_um_library_path);
            args.add(copy_string(to_string(libpath)));
        }
        
        if (win32_sdk.windows_sdk_ucrt_library_path) {
            snprintf(libpath, LINE_SIZE, "/libpath:%S", win32_sdk.windows_sdk_ucrt_library_path);
            args.add(copy_string(to_string(libpath)));
        }
        
        args.add(to_string("output.o"));
        args.add(to_string("msvcrt.lib"));
        args.add(to_string("kernel32.lib"));
        args.add(to_string("glfw3.lib"));
        args.add(to_string("user32.lib"));
        args.add(to_string("opengl32.lib"));
        args.add(to_string("shell32.lib"));
        args.add(to_string("gdi32.lib"));
        args.add(to_string("legacy_stdio_definitions.lib"));
        
        
        auto cmd_line = get_command_line(&args);
        printf("Linker line: %s\n", cmd_line);
        
        // system((char *)cmd_line);
        STARTUPINFOA startup;
        memset(&startup, 0, sizeof(STARTUPINFOA));
        startup.cb = sizeof(STARTUPINFOA);
        startup.dwFlags    = STARTF_USESTDHANDLES;
        startup.hStdInput  = GetStdHandle(STD_INPUT_HANDLE);
        startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
        startup.hStdError  = GetStdHandle(STD_ERROR_HANDLE);
        
        PROCESS_INFORMATION process_info;
        CreateProcessA(nullptr, (char *) cmd_line, nullptr, nullptr, TRUE, 0, nullptr, nullptr, &startup, &process_info);
        WaitForSingleObject(process_info.hProcess, INFINITE);
    }
#endif
    return 0;
}
