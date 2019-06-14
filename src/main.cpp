
#include "general.h"

#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include "llvm.h"
#include "sema.h"
#include "copier.h"

#ifdef WIN32
#include "microsoft_craziness.h"
#endif

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

void perform_load_from_string(Compiler *compiler, String source, Ast_Scope *target_scope) {
    Lexer *lexer = new Lexer(compiler, source, to_string(""));
    lexer->tokenize_text();
    
    if (compiler->errors_reported) return;
    
    Parser *parser = new Parser(lexer);
    parser->parse_scope(target_scope, false);
}

void perform_load(Compiler *compiler, String filename, Ast_Scope *target_scope) {
    String source;
    bool success = read_entire_file(filename, &source);
    if (!success) {
        printf("Could not open file: %.*s\n", (int)filename.length, filename.data);
        return;
    }
    
    Lexer *lexer = new Lexer(compiler, source, filename);
    lexer->tokenize_text();
    
    if (compiler->errors_reported) return;
    
    Parser *parser = new Parser(lexer);
    parser->parse_scope(target_scope, false);
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

const char *preload_text = R"C01N(

func __strings_match(a: string, b: string) -> bool {
    if (a.length != b.length) return false;
    if (a.data == null && b.data != null) return false;
    if (b.data == null && a.data != null) return false;
    if (a.data == null && b.data == null) return true;
    
    for 0..a.length-1 {
        if (a[it] != b[it]) return false;
    }

    return true;
}

)C01N";

extern "C" {
    Compiler *create_compiler_instance() {
        auto compiler = new Compiler();
        compiler->init();

        compiler->executable_name = to_string("output");

        compiler->copier = new Copier(compiler);

        compiler->sema = new Sema(compiler);

        compiler->llvm_gen = new LLVM_Generator(compiler);
        compiler->llvm_gen->init();

        perform_load_from_string(compiler, to_string((char *)preload_text), compiler->global_scope);

        return compiler;
    }

    bool compiler_run_default_link_command(Compiler *compiler) {
        if (compiler->executable_name == to_string("")) return false;
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
            args.add(to_string("/Fe:"));
            args.add(compiler->executable_name);
            
            
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
#else
        // @Incomplete should use the execpve family
        Array<String> args;
        args.add(to_string("ld"));
        args.add(to_string("output.o"));
        
        args.add(to_string("-o"));
        args.add(compiler->executable_name);
        
        args.add(to_string("-framework"));
        args.add(to_string("OpenGL"));
        
        args.add(to_string("-lglfw"));
        args.add(to_string("-lc"));
        
        auto cmd_line = get_command_line(&args);
        printf("Linker line: %s\n", cmd_line);
        system((char *)cmd_line);
#endif

        // @TODO make sure we successfully launch the link command and that it returns a success code
        return true;
    }

    bool compiler_load_file(Compiler *compiler, String filename) {
        perform_load(compiler, filename, compiler->global_scope);

        return compiler->errors_reported == 0;
    }

    bool compiler_typecheck_program(Compiler *compiler) {
        compiler->sema->typecheck_scope(compiler->global_scope);
        return compiler->errors_reported == 0;
    }

    bool compiler_generate_llvm_module(Compiler *compiler) {
        // @Incomplete global variables
    
        for (auto &function : compiler->function_emission_queue) {
            compiler->llvm_gen->emit_function(function);
        }

        return compiler->errors_reported == 0;
    }

    bool compiler_emit_object_file(Compiler *compiler) {
        compiler->llvm_gen->finalize();
        return compiler->errors_reported == 0;
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("ERROR: no input files\n");
        return -1;
    }
    
    String filename;
    bool is_metaprogram = false;
    
    for (int i = 1; i < argc; ++i) {
        if (to_string("-meta") == to_string(argv[i])) {
            is_metaprogram = true;
        } else {
            filename = to_string(argv[i]);
        }
    }
    
    auto compiler = create_compiler_instance();
    compiler->is_metaprogram = is_metaprogram;

    if (filename == to_string("")) {
        compiler->report_error((Token *)nullptr, "No input files specified.\n");
        return -1;
    }

    if (!compiler_load_file(compiler, filename)) return -1;

    if (!compiler_typecheck_program(compiler)) return -1;
    
    if (!compiler_generate_llvm_module(compiler)) return -1;
    
    if (compiler->is_metaprogram) {
        LLVM_Jitter *jitter = new LLVM_Jitter(compiler->llvm_gen);
        jitter->init();
        return 0;
    }
    
    if (!compiler_emit_object_file(compiler)) return -1;;

    if (!compiler_run_default_link_command(compiler)) return -1;

    return 0;
}
