library(RLLVMCompile)
f = function(x)  length(x) 
fc = compileFunction(f, Int32Type, REALSXPType, name = "f")

x = rnorm(10)
stopifnot(identical(.llvm(fc, x), length(x)))





