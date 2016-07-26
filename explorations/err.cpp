// Generated by llvm2cpp - DO NOT MODIFY!

#include <llvm/Pass.h>
#include <llvm/PassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/MathExtras.h>
#include <algorithm>
using namespace llvm;

Module* makeLLVMModule();

int main(int argc, char**argv) {
  Module* Mod = makeLLVMModule();
  verifyModule(*Mod, PrintMessageAction);
  PassManager PM;
  PM.add(createPrintModulePass(&outs()));
  PM.run(*Mod);
  return 0;
}


Module* makeLLVMModule() {
 // Module Construction
 Module* mod = new Module("err.ll", getGlobalContext());
 mod->setDataLayout("e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128");
 mod->setTargetTriple("x86_64-apple-macosx10.9.0");
 
 // Type Definitions
 ArrayType* ArrayTy_0 = ArrayType::get(IntegerType::get(mod->getContext(), 8), 3);
 
 PointerType* PointerTy_1 = PointerType::get(ArrayTy_0, 0);
 
 PointerType* PointerTy_3 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
 
 ArrayType* ArrayTy_2 = ArrayType::get(PointerTy_3, 2);
 
 PointerType* PointerTy_4 = PointerType::get(ArrayTy_2, 0);
 
 
 // Function Declarations
 
 // Global Variable Declarations

 
 GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod, 
 /*Type=*/ArrayTy_0,
 /*isConstant=*/true,
 /*Linkage=*/GlobalValue::PrivateLinkage,
 /*Initializer=*/0, // has initializer, specified below
 /*Name=*/".str");
 gvar_array__str->setAlignment(1);
 
 GlobalVariable* gvar_array__str1 = new GlobalVariable(/*Module=*/*mod, 
 /*Type=*/ArrayTy_0,
 /*isConstant=*/true,
 /*Linkage=*/GlobalValue::PrivateLinkage,
 /*Initializer=*/0, // has initializer, specified below
 /*Name=*/".str1");
 gvar_array__str1->setAlignment(1);
 
 GlobalVariable* gvar_array_ss = new GlobalVariable(/*Module=*/*mod, 
 /*Type=*/ArrayTy_2,
 /*isConstant=*/false,
 /*Linkage=*/GlobalValue::ExternalLinkage,
 /*Initializer=*/0, // has initializer, specified below
 /*Name=*/"ss");
 gvar_array_ss->setAlignment(16);
 
 // Constant Definitions
 Constant *const_array_5 = ConstantDataArray::getString(mod->getContext(), "s1", true);
 Constant *const_array_6 = ConstantDataArray::getString(mod->getContext(), "s2", true);
 std::vector<Constant*> const_array_7_elems;
 std::vector<Constant*> const_ptr_8_indices;
 ConstantInt* const_int32_9 = ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
 const_ptr_8_indices.push_back(const_int32_9);
 const_ptr_8_indices.push_back(const_int32_9);
 Constant* const_ptr_8 = ConstantExpr::getGetElementPtr(gvar_array__str, const_ptr_8_indices);
 const_array_7_elems.push_back(const_ptr_8);
 std::vector<Constant*> const_ptr_10_indices;
 const_ptr_10_indices.push_back(const_int32_9);
 const_ptr_10_indices.push_back(const_int32_9);
 Constant* const_ptr_10 = ConstantExpr::getGetElementPtr(gvar_array__str1, const_ptr_10_indices);
 const_array_7_elems.push_back(const_ptr_10);
 Constant* const_array_7 = ConstantArray::get(ArrayTy_2, const_array_7_elems);
 
 // Global Variable Definitions
 gvar_array__str->setInitializer(const_array_5);
 gvar_array__str1->setInitializer(const_array_6);
 gvar_array_ss->setInitializer(const_array_7);
 
 // Function Definitions
 
 return mod;
}