# This is a simplification of clangHandler which uses pointers
# as the parameter types, not CXCursor struct objects.
# This simplifies how the functions are called from the clang code and
# removes how we get the structs by value.

library(RLLVMCompile)

h = function(cur, parent, data)
{
#   ctr = ctr + 1L
#   kinds[ctr] = cur$kind
   kind = cur$kind
   printInt(kind)
   2L
#   CXChildVisit_Recurse
}

mod = Module()
createGlobalVariable("ctr", mod, Int32Type, createIntegerConstant(0L))
createGlobalVariable("kind", mod, Int32Type, createIntegerConstant(0L))
createGlobalVariable("kinds", mod, arrayType(Int32Type, 1000000))

cursorType = structType(list(kind = Int32Type, xdata = Int32Type, data = arrayType(Int8Type, 3L)), "CXCursor")
pointerCursorType = pointerType(cursorType)


library(RCIndex) # here because we need to find CXChildVisit_Recurse
llvmAddSymbol("printInt")
declareFunction(list(VoidType, Int32Type), "printInt", mod)

fc = compileFunction(h, Int32Type, list( pointerCursorType, pointerCursorType, pointerType(Int8Type)), module = mod, optimize = FALSE)

if(TRUE) {
   
 # ir = "tests/clang.ll"
 #  mod = parseIR(ir)

  
  ee = ExecutionEngine(mod)
  mod[["kind", ee = ee]]  
  fp = structure(getPointerToFunction(mod[["h"]], ee)@ref, class = "NativeSymbol")

  library(RCIndex)
  tu = createTU("testAlloc.c", includes = sprintf("%s/%s", R.home(), c("include", "../src/include")))
  .Call("R_clang_visitChildren_LLVM_test", as(tu, "CXCursor"), fp, FALSE)
  
  mod[["kind", ee = ee]] # should be 101
}
