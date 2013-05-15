# This is a simplification of clangHandler which uses pointers
# as the parameter types, not CXCursor struct objects.
# This simplifies how the functions are called from the clang code and
# removes how we get the structs by value.

library(RLLVMCompile)

h = function(cur, parent, data)
{
   kind = cur$xdata # kind
   if(cur$kind == CXCursor_CallExpr) {
     str = clang_getCursorSpelling(cur)
     names[ctr] = clang_getCString(str)
     ctr = ctr + 1L
   }
   
   CXChildVisit_Recurse
}

mod = Module()
mod[["ctr"]] = 1L
mod[["names"]] = arrayType(Int32Type, 1000000)


cursorType = structType(list(kind = Int32Type, xdata = Int32Type, data = arrayType(Int8Type, 3L)), "CXCursor")
pointerCursorType = pointerType(cursorType)

llvmAddSymbol("clang_getCursorSpelling", "clang_getCString")
declareFunction()

library(RCIndex) # here because we need to find CXChildVisit_Recurse
llvmAddSymbol("printInt")
declareFunction(list(VoidType, Int32Type), "printInt", mod)

fc = compileFunction(h, Int32Type, list( pointerCursorType, pointerCursorType, pointerType(Int8Type)), module = mod,
                      structInfo = list(CXCursor = cursorType))

if(FALSE) {
    # This commented out code is when we generate the IR from actual C code to see what llvm really generates for our
    # struct definition.
 # ir = "tests/clang.ll"
 #  mod = parseIR(ir)

  
  ee = ExecutionEngine(mod)
  mod[["kind", ee = ee]]  
  fp = structure(getPointerToFunction(mod[["h"]], ee)@ref, class = "NativeSymbol")

  library(RCIndex)
  tu = createTU("testAlloc.c", includes = sprintf("%s/%s", R.home(), c("include", "../src/include")))
  .Call("R_clang_visitChildren_LLVM_test", as(tu, "CXCursor"), fp, FALSE)
  
  mod[["kind", ee = ee]] # should be 101

  kinds = mod[["kinds", ee = ee]][ 1: mod[["ctr", ee = ee]] ]
  table(kinds)
  
}
