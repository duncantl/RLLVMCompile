library(Rllvm)

library(RCIndex)

if(FALSE) {
  mod = parseIR("clang.ll")
  funName = "bar"
} else {
  mod = parseIR("clangHandler2.mod")
  funName = "h"
}
#gn = Function("clang_CXCursor_getName", StringType, list(CXCursorType), mod)
llvmAddSymbol("clang_CXCursor_getName")

ee = ExecutionEngine(mod)
fp = structure(getPointerToFunction(mod[[ funName ]], ee)@ref, class = "NativeSymbol")

tu = createTU("../tests/testAlloc.c", includes = sprintf("%s/%s", R.home(), c("include", "../src/include")))
visitTU(tu, fp)



