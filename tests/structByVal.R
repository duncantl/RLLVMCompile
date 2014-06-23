library(RLLVMCompile)

# See ../explorations/clangHandler{,2}.R

h = function(cur, parent, data)
{
   ctr = ctr + 1L
   kind = cur$kind
   printf("%d\n", kind)
#   return(2L)
   2L
}

mod = Module()
mod[["ctr"]] = 1L
cursorType = structType(list(kind = Int32Type, xdata = Int32Type, data = arrayType(pointerType(Int8Type), 3L)), "CXCursor")

fc = compileFunction(h, Int32Type, list( cursorType, cursorType, pointerType(Int8Type)), module = mod,
                       structInfo = list(CXCursor = cursorType))

ee = ExecutionEngine(mod)
fp = structure(getPointerToFunction(fc, ee)@ref, class = "NativeSymbol")
library(RCIndex)
tu = createTU("testAlloc.c", includes = sprintf("%s/%s", R.home(), c("include", "../src/include")))
visitTU(tu, fp)

