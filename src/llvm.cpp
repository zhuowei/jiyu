

#include "llvm.h"
#include "ast.h"
#include "compiler.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

void LLVM_Generator::init() {
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();
    
    std::string TargetTriple = llvm::sys::getDefaultTargetTriple();
    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);
    
    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!Target) {
        errs() << Error;
        return;
    }
    
    auto CPU = "generic";
    auto Features = "";
    
    TargetOptions opt;
    auto RM = Optional<Reloc::Model>();
    TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);
    
    
    
    llvm_context = new LLVMContext();
    llvm_module = new Module("Htn Module", *llvm_context);
    irb = new IRBuilder<>(*llvm_context);
    
    type_void = Type::getVoidTy(*llvm_context);
    type_i1   = Type::getInt1Ty(*llvm_context);
    type_i8   = Type::getInt8Ty(*llvm_context);
    type_i16  = Type::getInt16Ty(*llvm_context);
    type_i32  = Type::getInt32Ty(*llvm_context);
    type_i64  = Type::getInt64Ty(*llvm_context);
    
    type_f32  = Type::getFloatTy(*llvm_context);
    type_f64  = Type::getDoubleTy(*llvm_context);
    
    type_string_length = nullptr;
    if (TargetMachine->getPointerSize(0) == 4) {
        type_string_length = type_i32;
    } else if (TargetMachine->getPointerSize(0) == 8) {
        type_string_length = type_i64;
    }
    
    assert(type_string_length);
    
    
    // Matches the definition in general.h
    type_string = StructType::create(*llvm_context, { type_i8->getPointerTo(), type_string_length }, "string", true/*packed*/);
}

void LLVM_Generator::finalize() {
    // @Cleanup duplicate
    std::string TargetTriple = llvm::sys::getDefaultTargetTriple();
    
    llvm_module->setDataLayout(TargetMachine->createDataLayout());
    llvm_module->setTargetTriple(TargetTriple);
    
    auto Filename = "output.o";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::F_None);
    
    if (EC) {
        errs() << "Could not open file: " << EC.message();
        return;
    }
    
    legacy::PassManager pass;
    auto FileType = TargetMachine::CGFT_ObjectFile;
    
    llvm_module->dump();
    
    pass.add(createVerifierPass(false));
    if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        errs() << "TargetMachine can't emit a file of this type";
        return;
    }
    
    pass.run(*llvm_module);
    dest.flush();
}

static StringRef string_ref(String s) {
    return StringRef(s.data, s.length);
}

Type *LLVM_Generator::get_type(Ast_Type_Info *type) {
    if (type->type == Ast_Type_Info::VOID) {
        return type_void;
    }
    
    if (type->type == Ast_Type_Info::INTEGER) {
        switch (type->size) {
            case 1: return type_i8;
            case 2: return type_i16;
            case 4: return type_i32;
            case 8: return type_i64;
            default: assert(false);
        }
    }
    
    if (type->type == Ast_Type_Info::BOOL) {
        return type_i1;
    }
    
    if (type->type == Ast_Type_Info::FLOAT) {
        switch(type->size) {
            case 4: return type_f32;
            case 8: return type_f64;
            default: assert(false);
        }
    }
    
    if (type->type == Ast_Type_Info::STRING) {
        return type_string;
    }
    
    if (type->type == Ast_Type_Info::POINTER) {
        auto pointee = get_type(type->pointer_to);
        return pointee->getPointerTo();
    }
    
    assert(false);
    return nullptr;
}

FunctionType *LLVM_Generator::create_function_type(Ast_Function *function) {
    Array<Type *> arguments;
    
    for (auto &arg : function->arguments) {
        if (arg->type_info == compiler->type_void) continue;
        
        Type *type = get_type(arg->type_info);
        arguments.add(type);
    }
    
    for (auto &ret : function->returns) {
        if (ret->type_info == compiler->type_void) continue;
        Type *type = get_type(ret->type_info)->getPointerTo();
        arguments.add(type);
    }
    return FunctionType::get(type_void, ArrayRef<Type *>(arguments.data, arguments.count), function->is_c_varargs);
}

Value *LLVM_Generator::get_value_for_decl(Ast_Declaration *decl) {
    for (auto &it : decl_value_map) {
        if (it.item1 == decl) {
            return it.item2;
        }
    }
    
    return nullptr;
}

static Value *create_alloca_in_entry(IRBuilder<> *irb, Type *type) {
    auto current_block = irb->GetInsertBlock();
    
    auto func = current_block->getParent();
    
    BasicBlock *entry = &func->getEntryBlock();
    irb->SetInsertPoint(entry->getTerminator());
    Value *alloca = irb->CreateAlloca(type);
    
    irb->SetInsertPoint(current_block);
    
    return alloca;
}

Value *LLVM_Generator::create_string_literal(Ast_Literal *lit) {
    assert(lit->literal_type == Ast_Literal::STRING);
    
    bool add_null = true;
    Constant *data = irb->CreateGlobalStringPtr(string_ref(lit->string_value));
    Constant *length = ConstantInt::get(type_string_length, lit->string_value.length);
    
    return ConstantStruct::get(type_string, { data, length });
}

Value *LLVM_Generator::dereference(Value *value, s64 element_path_index, bool is_lvalue) {
    // @TODO I think ideally, the front-end would change and dereferences of constant values with replaecments of literals of the value so that we can simplify LLVM code generation
    if (auto constant = dyn_cast<Constant>(value)) {
        return irb->CreateExtractValue(constant, element_path_index);
    } else {
        auto valueptr = irb->CreateGEP(value, { ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, element_path_index) });
        if (!is_lvalue) return irb->CreateLoad(valueptr);
        return valueptr;
    }
}

Value *LLVM_Generator::emit_expression(Ast_Expression *expression, bool is_lvalue) {
    switch (expression->type) {
        case AST_SCOPE: {
            auto scope = static_cast<Ast_Scope *>(expression);
            emit_scope(scope);
            
            return nullptr;
        }
        
        case AST_UNARY_EXPRESSION: {
            auto un = static_cast<Ast_Unary_Expression *>(expression);
            
            if (un->operator_type == Token::STAR) {
                auto value = emit_expression(un->expression, true);
                return value;
            } else if (un->operator_type == Token::DEREFERENCE_OR_SHIFT) {
                auto value = emit_expression(un->expression, is_lvalue);
                
                if (!is_lvalue) value = irb->CreateLoad(value);
                return value;
            }
            
            assert(false);
            break;
        }
        
        case AST_BINARY_EXPRESSION: {
            auto bin = static_cast<Ast_Binary_Expression *>(expression);
            
            if (bin->operator_type == Token::EQUALS) {
                Value *left  = emit_expression(bin->left,  true);
                Value *right = emit_expression(bin->right, false);
                
                irb->CreateStore(right, left);
                return nullptr;
            } else {
                Value *left  = emit_expression(bin->left,  false);
                Value *right = emit_expression(bin->right, false);
                
                assert(types_match(bin->left->type_info, bin->right->type_info));
                // @TODO NUW NSW?
                switch (bin->operator_type) {
                    case Token::STAR:    return irb->CreateMul(left, right);
                    case Token::PERCENT: assert(false); // @Incomplete
                    case Token::SLASH: {
                        auto info = bin->left->type_info;
                        if (info->type == Ast_Type_Info::INTEGER) {
                            if (info->is_signed) {
                                return irb->CreateSDiv(left, right);
                            } else {
                                return irb->CreateUDiv(left, right);
                            }
                        } else {
                            assert(info->type == Ast_Type_Info::FLOAT);
                            return irb->CreateFDiv(left, right);
                        }
                    }
                    case Token::PLUS:  return irb->CreateAdd(left, right);
                    case Token::MINUS: return irb->CreateSub(left, right);
                    case Token::EQ_OP: return irb->CreateICmpEQ(left, right);
                    case Token::NE_OP: return irb->CreateICmpNE(left, right);
                    default: assert(false);
                }
            }
            
            assert(false);
        }
        
        case AST_LITERAL: {
            auto lit = static_cast<Ast_Literal *>(expression);
            
            switch (lit->literal_type) {
                case Ast_Literal::INTEGER: return ConstantInt::get(get_type(lit->type_info), lit->integer_value, lit->type_info->is_signed);
                case Ast_Literal::STRING:  return create_string_literal(lit);
                case Ast_Literal::FLOAT:   return ConstantFP::get(get_type(lit->type_info),  lit->float_value);
                case Ast_Literal::BOOL:    return ConstantInt::get(get_type(lit->type_info), (lit->bool_value ? 1 : 0));
                default: return nullptr;
            }
        }
        
        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            assert(ident->resolved_declaration);
            
            assert(ident->resolved_declaration->type == AST_DECLARATION);
            auto decl = static_cast<Ast_Declaration *>(ident->resolved_declaration);
            
            auto value = get_value_for_decl(decl);
            
            if (!is_lvalue) return irb->CreateLoad(value);
            
            return value;
        }
        
        case AST_DECLARATION: {
            auto decl = static_cast<Ast_Declaration *>(expression);
            if (decl->initializer_expression) {
                auto value = emit_expression(decl->initializer_expression);
                irb->CreateStore(value, get_value_for_decl(decl));
            }
            return nullptr;
        }
        
        case AST_FUNCTION_CALL: {
            auto call = static_cast<Ast_Function_Call *>(expression);
            
            Array<Value *> args;
            for (auto &it : call->argument_list) {
                auto value = emit_expression(it);
                args.add(value);
            }
            
            auto target_function = static_cast<Ast_Function *>(call->identifier->resolved_declaration);
            auto func = get_or_create_function(target_function);
            
            assert(func);
            // func->dump();
            return irb->CreateCall(func, ArrayRef<Value *>(args.data, args.count));
        }
        
        case AST_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            
            auto lhs = emit_expression(deref->left, true);
            
            assert(deref->element_path_index >= 0);
            assert(deref->byte_offset >= 0);
            
            auto value = dereference(lhs, deref->element_path_index, is_lvalue);
            return value;
		}
        
        case AST_CAST: {
            auto cast = static_cast<Ast_Cast *>(expression);
            Value *value = emit_expression(cast->expression);
            
            auto src = cast->expression->type_info;
            auto dst = cast->type_info;
            
            auto src_type = get_type(src);
            auto dst_type = get_type(dst);
            if (is_int_type(src) && is_int_type(dst)) {
                if (src->size > dst->size) {
                    return irb->CreateTrunc(value, dst_type);
                } else if (src->size < dst->size) {
                    if (src->is_signed && dst->is_signed) {
                        return irb->CreateSExt(value, dst_type);
                    } else {
                        return irb->CreateZExt(value, dst_type);
                    }
                }
            } else if (is_float_type(src) && is_float_type(dst)) {
                if (src->size < dst->size) {
                    return irb->CreateFPExt(value, dst_type);
                } else if (src->size > dst->size) {
                    return irb->CreateFPTrunc(value, dst_type);
                }
            }
            
            assert(false);
            break;
        }
        
        case AST_IF: {
            auto _if = static_cast<Ast_If *>(expression);
            auto cond = emit_expression(_if->condition);
            
            auto current_block = irb->GetInsertBlock();
            
            BasicBlock *next_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            BasicBlock *then_block = nullptr;
            BasicBlock *else_block = nullptr;
            
            BasicBlock *failure_target = next_block;
            
            then_block = BasicBlock::Create(*llvm_context, "then_target", current_block->getParent());
            if (_if->then_statement) {
                irb->SetInsertPoint(then_block);
                emit_expression(_if->then_statement);
                irb->SetInsertPoint(then_block);
            }
            irb->CreateBr(next_block);
            
            if (_if->else_statement) {
                else_block = BasicBlock::Create(*llvm_context, "else_target", current_block->getParent());
                irb->SetInsertPoint(else_block);
                emit_expression(_if->else_statement);
                irb->SetInsertPoint(else_block);
                irb->CreateBr(next_block);
                
                failure_target = else_block;
            }
            
            irb->SetInsertPoint(current_block);
            irb->CreateCondBr(cond, then_block, failure_target);
            irb->SetInsertPoint(next_block);
            
            break;
        }
        
        case AST_WHILE: {
            auto loop = static_cast<Ast_While *>(expression);
            
            auto current_block = irb->GetInsertBlock();
            
            BasicBlock *next_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            BasicBlock *loop_header = BasicBlock::Create(*llvm_context, "loop_header", current_block->getParent());
            BasicBlock *loop_body = BasicBlock::Create(*llvm_context, "loop_body", current_block->getParent());
            
            
            irb->CreateBr(loop_header);
            
            irb->SetInsertPoint(loop_header);
            // emit the condition in the loop header so that it always executes when we loop back around
            auto cond = emit_expression(loop->condition);
            irb->SetInsertPoint(loop_header);
            irb->CreateCondBr(cond, loop_body, next_block);
            
            irb->SetInsertPoint(loop_body);
            if (loop->statement) {
                emit_expression(loop->statement);
                irb->SetInsertPoint(loop_body);
                irb->CreateBr(loop_header);
            }
            
            irb->SetInsertPoint(next_block);
            break;
        }
    }
    
    return nullptr;
}

Function *LLVM_Generator::get_or_create_function(Ast_Function *function) {
    assert(function->identifier);
    String name = function->identifier->name->name;
    
    auto func = llvm_module->getFunction(string_ref(name));
    
    if (!func) {
        FunctionType *function_type = create_function_type(function);
        func = Function::Create(function_type, GlobalValue::LinkageTypes::ExternalLinkage, string_ref(name), llvm_module);
        
        array_count_type i = 0;
        for (auto &a : func->args()) {
            if (i < function->arguments.count) {
                a.setName(string_ref(function->arguments[i]->identifier->name->name));
                
                ++i;
            }
        }
    }
    
    return func;
}

void LLVM_Generator::emit_scope(Ast_Scope *scope) {
    // setup variable mappings
    for (auto &it : scope->declarations) {
        if (it->type != AST_DECLARATION) continue;
        auto decl = static_cast<Ast_Declaration *>(it);
        
        auto alloca = create_alloca_in_entry(irb, get_type(it->type_info));
        
        if (decl->identifier) {
            alloca->setName(string_ref(decl->identifier->name->name));
        }
        
        assert(get_value_for_decl(decl) == nullptr);
        decl_value_map.add(MakeTuple(decl, alloca));
    }
    
    for (auto &it : scope->statements) {
        emit_expression(it);
    }
}

void LLVM_Generator::emit_function(Ast_Function *function) {
    assert(function->identifier && function->identifier->name);
    
    Function *func = get_or_create_function(function);
    if (!function->scope) return; // forward declaration of external thing
    
    // create entry block
    BasicBlock *entry = BasicBlock::Create(*llvm_context, "entry", func);
    BasicBlock *starting_block = BasicBlock::Create(*llvm_context, "start", func);
    
    irb->SetInsertPoint(entry);
    irb->CreateBr(starting_block);
    
    irb->SetInsertPoint(starting_block);
    emit_scope(function->scope);
    irb->CreateRetVoid();
    
    // func->dump();
    decl_value_map.clear();
}

