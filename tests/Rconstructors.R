library(RLLVMCompile)
g = function() { x = integer(); x}
fun = compileFunction(g, Int32Type)

g = function() { x = logical(); x}
fun = compileFunction(g, Int1Type)
.llvm(fun)

