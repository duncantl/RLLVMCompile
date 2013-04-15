library(RLLVMCompile)
f = function(x) { length(x) }
fc = compileFunction(f, Int32Type, list(Rllvm:::getSEXPType("REAL")), name = "f")



