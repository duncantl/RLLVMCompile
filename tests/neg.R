library(RLLVMCompile)
f = function(n) +n
fun = compileFunction(f, Int32Type, Int32Type)
.llvm(fun, 10)

g = function(n) -n
gun = compileFunction(g, Int32Type, Int32Type)
.llvm(gun, 10)




