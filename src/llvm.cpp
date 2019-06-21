

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

#include "llvm/IR/IRBuilder.h"

#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/JITSymbol.h"

#include "llvm/Transforms/Utils/Cloning.h"

using namespace llvm;
using namespace llvm::orc;

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
    
    
    
    auto ctx = llvm::make_unique<LLVMContext>();
    thread_safe_context = new ThreadSafeContext(std::move(ctx));
    llvm_context = thread_safe_context->getContext();
    
    llvm_module = new Module("jiyu Module", *llvm_context);
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
        type_intptr = type_i32;
    } else if (TargetMachine->getPointerSize(0) == 8) {
        type_string_length = type_i64;
        type_intptr = type_i64;
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
    
    // llvm_module->dump();
    
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
        // return type_i8 for pointers.
        return type_i8;
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
    
    if (type->type == Ast_Type_Info::ARRAY) {
        auto element = get_type(type->array_element);
        if (type->array_element_count >= 0) {
            assert(type->is_dynamic == false);
            
            return ArrayType::get(element, type->array_element_count);
        } else {
            // @Cleanup this should be type_array_count or something
            auto count = type_string_length;
            auto data  = element->getPointerTo();
            
            if (!type->is_dynamic) {
                return StructType::get(*llvm_context, {data, count}, false);
            } else {
                auto allocated = count;
                return StructType::get(*llvm_context, {data, count, allocated}, false);
            }
        }
    }
    
    if (type->type == Ast_Type_Info::STRUCT) {
        Array<Type *> member_types;
        
        for (auto member : type->struct_members) {
            if (member.is_let) continue;
            
            member_types.add(get_type(member.type_info));
        }
        
        return StructType::get(*llvm_context, ArrayRef<Type *>(member_types.data, member_types.count), false);
    }
    
    assert(false);
    return nullptr;
}

FunctionType *LLVM_Generator::create_function_type(Ast_Function *function) {
    Array<Type *> arguments;
    Type *return_type = type_void;
    
    for (auto &arg : function->arguments) {
        auto arg_type = get_type_info(arg);
        
        if (arg_type  == compiler->type_void) continue;
        
        Type *type = get_type(arg_type);
        arguments.add(type);
    }
    
    if (function->return_decl) {
        return_type = get_type(get_type_info(function->return_decl));
    }
    
    return FunctionType::get(return_type, ArrayRef<Type *>(arguments.data, arguments.count), function->is_c_varargs);
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
    
    if (lit->string_value.length == 0 || lit->string_value.data == nullptr) {
        return Constant::getNullValue(type_string);
    }
    
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
        // @Cleanup type_i32 use for array indexing?
        auto valueptr = irb->CreateGEP(value, { ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, element_path_index) });
        if (!is_lvalue) return irb->CreateLoad(valueptr);
        return valueptr;
    }
}

void LLVM_Generator::default_init_struct(Value *decl_value, Ast_Type_Info *info) {
    assert(info->type == Ast_Type_Info::STRUCT);
    assert(info->struct_decl);

    auto _struct = info->struct_decl;

    auto null_value = Constant::getNullValue(get_type(info));
    assert(null_value->getType() == decl_value->getType()->getPointerElementType());
    irb->CreateStore(null_value, decl_value);

    s32 element_path_index = 0;
    for (auto member: _struct->member_scope.declarations) {
        if (member->type == AST_DECLARATION) {
            auto decl = static_cast<Ast_Declaration *>(member);

            if (decl->is_let) continue;
            assert(decl->is_struct_member);

            if (decl->initializer_expression) {
                auto expr = emit_expression(decl->initializer_expression);

                auto gep = dereference(decl_value, element_path_index, true);
                irb->CreateStore(expr, gep);
            } else {
                auto mem_info = get_type_info(decl);
                if (mem_info->type == Ast_Type_Info::STRUCT) {
                    auto gep = dereference(decl_value, element_path_index, true);
                    default_init_struct(gep, mem_info);
                }
            }

            element_path_index++;
        }
    }
}

Value *LLVM_Generator::emit_expression(Ast_Expression *expression, bool is_lvalue) {
    while(expression->substitution) expression = expression->substitution;
    
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
                
                value = irb->CreateLoad(value);
                return value;
            } else if (un->operator_type == Token::MINUS) {
                auto value = emit_expression(un->expression);
                auto type = get_type_info(un->expression);
                
                if (type->type == Ast_Type_Info::INTEGER) {
                    return irb->CreateNeg(value);
                } else if (type->type == Ast_Type_Info::FLOAT) {
                    return irb->CreateFNeg(value);
                }
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
                
                // @TODO NUW NSW?
                switch (bin->operator_type) {
                    case Token::STAR: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            return irb->CreateMul(left, right);
                        } else {
                            assert(info->type == Ast_Type_Info::FLOAT);
                            return irb->CreateFMul(left, right);
                        }
                    }
                    case Token::PERCENT: assert(false); // @Incomplete
                    case Token::SLASH: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
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
                    case Token::PLUS: {
                        auto left_type = get_type_info(bin->left);
                        auto right_type = get_type_info(bin->right);
                        
                        if (is_pointer_type(left_type) &&
                            is_int_type(right_type)) {
                            return irb->CreateGEP(left, {right});
                        } else if (is_pointer_type(left_type) && is_pointer_type(right_type)) {
                            Value *left_int  = irb->CreatePtrToInt(left,  type_intptr);
                            Value *right_int = irb->CreatePtrToInt(right, type_intptr);
                            
                            Value *result = irb->CreateAdd(left_int, right_int);
                            return irb->CreateIntToPtr(result, get_type(left_type));
                        } else if (is_float_type(left_type)) {
                            assert(is_float_type(right_type));
                            
                            return irb->CreateFAdd(left, right);
                        }
                        
                        return irb->CreateAdd(left, right);
                    }
                    case Token::MINUS: {
                        auto left_type  = get_type_info(bin->left);
                        auto right_type = get_type_info(bin->right);
                        
                        if (is_pointer_type(left_type) && is_pointer_type(right_type)) {
                            Value *left_int  = irb->CreatePtrToInt(left,  type_intptr);
                            Value *right_int = irb->CreatePtrToInt(right, type_intptr);
                            
                            Value *result = irb->CreateSub(left_int, right_int);
                            return irb->CreateIntToPtr(result, get_type(left_type));
                        }
                        
                        return irb->CreateSub(left, right);
                    }
                    case Token::EQ_OP: {
                        return irb->CreateICmpEQ(left, right);
                    }
                    case Token::NE_OP: return irb->CreateICmpNE(left, right);
                    case Token::LE_OP: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateICmpSLE(left, right);
                            } else {
                                return irb->CreateICmpULE(left, right);
                            }
                        } else {
                            assert(info->type == Ast_Type_Info::FLOAT);
                            return irb->CreateFCmpULE(left, right);
                        }
                    }
                    case Token::GE_OP: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateICmpSGE(left, right);
                            } else {
                                return irb->CreateICmpUGE(left, right);
                            }
                        } else {
                            assert(info->type == Ast_Type_Info::FLOAT);
                            return irb->CreateFCmpUGE(left, right);
                        }
                    }
                    
                    case '<': {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateICmpSLT(left, right);
                            } else {
                                return irb->CreateICmpULT(left, right);
                            }
                        } else {
                            assert(info->type == Ast_Type_Info::FLOAT);
                            return irb->CreateFCmpULT(left, right);
                        }
                    }
                    
                    case '>': {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateICmpSGT(left, right);
                            } else {
                                return irb->CreateICmpUGT(left, right);
                            }
                        } else {
                            assert(info->type == Ast_Type_Info::FLOAT);
                            return irb->CreateFCmpUGT(left, right);
                        }
                    }
                    
                    case Token::VERTICAL_BAR: {
                        return irb->CreateOr(left, right);
                    }
                    
                    case Token::AND_OP: {
                        assert(left->getType()  == type_i1);
                        assert(right->getType() == type_i1);
                        
                        return irb->CreateAnd(left, right);
                    }
                    default: assert(false);
                }
            }
            
            assert(false);
        }
        
        case AST_LITERAL: {
            auto lit = static_cast<Ast_Literal *>(expression);
            
            auto type_info = get_type_info(lit);
            switch (lit->literal_type) {
                case Ast_Literal::INTEGER: return ConstantInt::get(get_type(type_info), lit->integer_value, type_info->is_signed);
                case Ast_Literal::STRING:  return create_string_literal(lit);
                case Ast_Literal::FLOAT:   return ConstantFP::get(get_type(type_info),  lit->float_value);
                case Ast_Literal::BOOL:    return ConstantInt::get(get_type(type_info), (lit->bool_value ? 1 : 0));
                case Ast_Literal::NULLPTR: return ConstantPointerNull::get(static_cast<PointerType *>(get_type(type_info)));
                default: return nullptr;
            }
        }
        
        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            assert(ident->resolved_declaration);
            
            assert(ident->resolved_declaration->type == AST_DECLARATION);
            auto decl = static_cast<Ast_Declaration *>(ident->resolved_declaration);
            
            if (decl->is_let && !decl->is_readonly_variable) {
                return emit_expression(decl->initializer_expression);
            }
            
            auto value = get_value_for_decl(decl);
            
            if (!is_lvalue) return irb->CreateLoad(value);
            
            return value;
        }
        
        case AST_DECLARATION: {
            auto decl = static_cast<Ast_Declaration *>(expression);
            auto decl_value = get_value_for_decl(decl);
            
            if (decl->initializer_expression) {
                auto value = emit_expression(decl->initializer_expression);
                irb->CreateStore(value, decl_value);
            } else {
                // if a declaration does not have an initializer, initialize to 0
                auto type_info = get_type_info(decl);
                if (type_info->type == Ast_Type_Info::STRUCT) {
                    default_init_struct(decl_value, type_info);
                } else {
                    auto type = decl_value->getType()->getPointerElementType();
                    irb->CreateStore(Constant::getNullValue(type), decl_value);
                }
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
            
            // promote C vararg arguments if they are not the required sizes
            // for ints, this typically means promoting i8 and i16 to i32.
            if (target_function->is_c_varargs) {
                for (array_count_type i = target_function->arguments.count; i < call->argument_list.count; ++i) {
                    auto value = args[i];
                    auto arg   = call->argument_list[i];
                    
                    auto type = value->getType();
                    if (type->isIntegerTy() && type->getPrimitiveSizeInBits() < type_i32->getPrimitiveSizeInBits()) {
                        assert(get_type_info(arg)->type == Ast_Type_Info::INTEGER ||
                                get_type_info(arg)->type == Ast_Type_Info::BOOL);
                        if (get_type_info(arg)->is_signed) {
                            args[i] = irb->CreateSExt(value, type_i32);
                        } else {
                            args[i] = irb->CreateZExt(value, type_i32);
                        }
                    } else if (type->isFloatTy() && type->getPrimitiveSizeInBits() < type_f64->getPrimitiveSizeInBits()) {
                        assert(get_type_info(arg)->type == Ast_Type_Info::FLOAT);
                        args[i] = irb->CreateFPExt(value, type_f64);
                    }
                }
            }
            
            auto func = get_or_create_function(target_function);
            
            assert(func);
            return irb->CreateCall(func, ArrayRef<Value *>(args.data, args.count));
        }
        
        case AST_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            
            auto lhs = emit_expression(deref->left, true);
            
            assert(deref->element_path_index >= 0);
            
            // @Incomplete
            // assert(deref->byte_offset >= 0);
            
            auto value = dereference(lhs, deref->element_path_index, is_lvalue);
            return value;
        }
        
        case AST_CAST: {
            auto cast = static_cast<Ast_Cast *>(expression);
            Value *value = emit_expression(cast->expression);
            
            auto src = get_type_info(cast->expression);
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
                
                assert(src_type == dst_type);
                return value;
            } else if (is_float_type(src) && is_float_type(dst)) {
                if (src->size < dst->size) {
                    return irb->CreateFPExt(value, dst_type);
                } else if (src->size > dst->size) {
                    return irb->CreateFPTrunc(value, dst_type);
                }
                
                assert(src_type == dst_type);
                return value;
            } else if (is_float_type(src) && is_int_type(dst)) {
                if (dst->is_signed) {
                    return irb->CreateFPToSI(value, dst_type);
                } else {
                    return irb->CreateFPToUI(value, dst_type);
                }
            } else if (is_int_type(src) && is_float_type(dst)) {
                if (src->is_signed) {
                    return irb->CreateSIToFP(value, dst_type);
                } else {
                    return irb->CreateUIToFP(value, dst_type);
                }
            } else if (is_pointer_type(src) && is_pointer_type(dst)) {
                return irb->CreatePointerCast(value, dst_type);
            } else if (is_pointer_type(src) && is_int_type(dst)) {
                return irb->CreatePtrToInt(value, dst_type);
            } else if (is_int_type(src) && is_pointer_type(dst)) {
                return irb->CreateIntToPtr(value, dst_type);
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
                
            }
            if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(next_block);
            
            if (_if->else_statement) {
                else_block = BasicBlock::Create(*llvm_context, "else_target", current_block->getParent());
                irb->SetInsertPoint(else_block);
                emit_expression(_if->else_statement);
                
                if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(next_block);
                
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
            
            BasicBlock *next_block = BasicBlock::Create(*llvm_context, "loop_exit", current_block->getParent());
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
                // irb->SetInsertPoint(loop_body);
                if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(loop_header);
            }
            
            irb->SetInsertPoint(next_block);
            break;
        }
        
        case AST_FOR: {
            auto _for = static_cast<Ast_For *>(expression);
            
            auto it_decl = _for->iterator_decl;
            auto it_alloca = create_alloca_in_entry(irb, get_type(get_type_info(it_decl)));
            auto decl_type = get_type_info(it_decl);
            
            decl_value_map.add(MakeTuple(it_decl, it_alloca));
            
            auto it_index_decl = _for->iterator_index_decl;
            Ast_Type_Info *it_index_type = nullptr;
            Value *it_index_alloca = nullptr;
            if (it_index_decl) {
                it_index_type = get_type_info(it_index_decl);
                it_index_alloca = create_alloca_in_entry(irb, get_type(it_index_type));
                decl_value_map.add(MakeTuple(it_index_decl, it_index_alloca));
                emit_expression(it_index_decl);
            } else {
                it_index_type = decl_type;
                it_index_alloca = it_alloca;
                
                emit_expression(it_decl);
            }
            
            auto current_block = irb->GetInsertBlock();
            
            BasicBlock *next_block = BasicBlock::Create(*llvm_context, "for_exit", current_block->getParent());
            BasicBlock *loop_header = BasicBlock::Create(*llvm_context, "for_header", current_block->getParent());
            BasicBlock *loop_body = BasicBlock::Create(*llvm_context, "for_body", current_block->getParent());
            
            
            irb->CreateBr(loop_header);
            
            irb->SetInsertPoint(loop_header);
            // emit the condition in the loop header so that it always executes when we loop back around
            auto it_index = irb->CreateLoad(it_index_alloca);
            assert(it_index_type->type == Ast_Type_Info::INTEGER);
            
            auto upper = emit_expression(_for->upper_range_expression);
            Value *cond = nullptr;
            if (it_index_decl) {
                // use < here otherwise, we'll overstep by one.
                // @Cleanup maybe this should be flagged as a half-open loop
                // when we support that?
                if (it_index_type->is_signed) {
                    cond = irb->CreateICmpSLT(it_index, upper);
                } else {
                    cond = irb->CreateICmpULT(it_index, upper);
                }
            } else {
                if (it_index_type->is_signed) {
                    cond = irb->CreateICmpSLE(it_index, upper);
                } else {
                    cond = irb->CreateICmpULE(it_index, upper);
                }
            }
            
            irb->SetInsertPoint(loop_header);
            irb->CreateCondBr(cond, loop_body, next_block);
            
            irb->SetInsertPoint(loop_body);
            if (it_index_decl) {
                emit_expression(it_decl);
            }
            
            emit_scope(&_for->body);
            
            irb->CreateStore(irb->CreateAdd(it_index, ConstantInt::get(get_type(it_index_type), 1)), it_index_alloca);
            if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(loop_header);
            
            irb->SetInsertPoint(next_block);
            break;
        }
        
        case AST_RETURN: {
            auto ret = static_cast<Ast_Return *>(expression);
            if (ret->expression) {
                auto value = emit_expression(ret->expression);
                assert(value);
                irb->CreateRet(value);
            } else {
                irb->CreateRetVoid();
            }
            
            // Create a new block so subsequent instructions have some where to generate to
            // @TODO Actually, Idk if this is correct, will have to test with how ifs and loops work...
            
            /*
            auto current_block = irb->GetInsertBlock();
            BasicBlock *new_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            
            irb->SetInsertPoint(new_block);
            */
            break;
        }
        
        case AST_ARRAY_DEREFERENCE: {
            auto deref = static_cast<Ast_Array_Dereference *>(expression);
            
            auto array = emit_expression(deref->array_or_pointer_expression, true);
            auto index = emit_expression(deref->index_expression);
            
            auto type = get_type_info(deref->array_or_pointer_expression);
            if (type->type == Ast_Type_Info::ARRAY && type->array_element_count == -1) {
                // @Cleanup hardcoded indices
                array = irb->CreateGEP(array, {ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, 0)});
                array = irb->CreateLoad(array);
                auto element = irb->CreateGEP(array, index);
                
                if (!is_lvalue) return irb->CreateLoad(element);
                return element;
            } else if (type->type == Ast_Type_Info::STRING) {
                // @Note although this is identical to the dynamic/static array case,
                // I've chosen to duplicate the code in case we chnage the order of
                // any of these implicit struct fields.
                // @Cleanup hardcoded indices.
                array = irb->CreateGEP(array, {ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, 0)});
                array = irb->CreateLoad(array);
                auto element = irb->CreateGEP(array, index);
                
                if (!is_lvalue) return irb->CreateLoad(element);
                return element;
            } else if (type->type == Ast_Type_Info::POINTER) {
                auto ptr = irb->CreateLoad(array);
                auto element = irb->CreateGEP(ptr, {index});
                
                if (!is_lvalue) return irb->CreateLoad(element);
                return element;
            }
            
            // @Cleanup type_i32 use for array indexing
            auto element = irb->CreateGEP(array, {ConstantInt::get(type_i32, 0), index});
            
            if (!is_lvalue) return irb->CreateLoad(element);
            return element;
        }
    }
    
    return nullptr;
}

Function *LLVM_Generator::get_or_create_function(Ast_Function *function) {
    assert(function->identifier);
    String linkage_name = function->linkage_name;
    
    auto func = llvm_module->getFunction(string_ref(linkage_name));
    
    if (!func) {
        FunctionType *function_type = create_function_type(function);
        func = Function::Create(function_type, GlobalValue::LinkageTypes::ExternalLinkage, string_ref(linkage_name), llvm_module);
        
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
    for (auto it : scope->declarations) {
        while (it->substitution) it = it->substitution;
        
        if (it->type != AST_DECLARATION) continue;
        auto decl = static_cast<Ast_Declaration *>(it);
        
        auto alloca = create_alloca_in_entry(irb, get_type(get_type_info(it)));
        
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
    
    auto arg_it = func->arg_begin();
    for (array_count_type i = 0; i < function->arguments.count; ++i) {
        auto a  = arg_it;
        
        auto decl = function->arguments[i];
        Value *alloca = irb->CreateAlloca(get_type(get_type_info(decl)));
        
        if (decl->identifier) {
            alloca->setName(string_ref(decl->identifier->name->name));
        }
        
        irb->CreateStore(a, alloca);
        
        assert(get_value_for_decl(decl) == nullptr);
        decl_value_map.add(MakeTuple(decl, alloca));
        
        arg_it++;
    }
    
    irb->CreateBr(starting_block);
    
    irb->SetInsertPoint(starting_block);
    emit_scope(function->scope);
    
    auto current_block = irb->GetInsertBlock();
    
    if (!current_block->getTerminator()) {
        auto return_decl = function->return_decl;
        if (return_decl) {
            irb->CreateRet(Constant::getNullValue(get_type(get_type_info(return_decl))));
        } else {
            irb->CreateRetVoid();
        }
    }
    // func->dump();
    decl_value_map.clear();
}

#include <stdio.h>

void LLVM_Jitter::init() {
    llvm::sys::DynamicLibrary::LoadLibraryPermanently("OpenGL");
    llvm::sys::DynamicLibrary::LoadLibraryPermanently("/usr/local/Cellar/glfw/3.3/lib/libglfw.dylib");
    auto JTMB = JITTargetMachineBuilder::detectHost();
    
    if (!JTMB) {
        JTMB.takeError();
        return;
    }
    
    auto DL = JTMB->getDefaultDataLayoutForTarget();
    if (!DL) {
        DL.takeError();
        return;
    }
    
    ExecutionSession ES;
    RTDyldObjectLinkingLayer ObjectLayer(ES, []() { return llvm::make_unique<SectionMemoryManager>(); });

#ifdef WIN32
    ObjectLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);
#endif

    IRCompileLayer CompileLayer(ES, ObjectLayer, ConcurrentIRCompiler(*JTMB));
    
    MangleAndInterner Mangle(ES, *DL);
    
    ES.getMainJITDylib().setGenerator(cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(*DL)));
    
    llvm->llvm_module->setDataLayout(*DL);
    // llvm->llvm_module->dump();
    
    if (!llvm->llvm_module->getFunction("main")) {
        compiler->report_error((Token *)nullptr, "No main function defined for meta program. Aborting.\n");
        return;
    }
    
    cantFail(CompileLayer.add(ES.getMainJITDylib(),
                              ThreadSafeModule(std::unique_ptr<Module>(llvm->llvm_module), *llvm->thread_safe_context)));
    
    auto sym = ES.lookup({&ES.getMainJITDylib()}, Mangle("main"));
    if (!sym) {
        sym.takeError();
        return;
    }
    
    auto *Main = (void (*)()) sym->getAddress();
    Main();
}

void *LLVM_Jitter::lookup_symbol(String name) {
    return nullptr;
}