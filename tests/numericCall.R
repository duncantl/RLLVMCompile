library(RLLVMCompile)
f = function(n) {  x = numeric(n) ; x[1] = 1; x}
fc = compileFunction(f, REALSXPType, Int32Type)
.llvm(fc, 10)

