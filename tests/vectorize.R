library(RLLVMCompile)

f = function(x) return(x + 1)

fc = compileFunction(f, DoubleType, DoubleType, .vectorize = "x", asFunction = TRUE)
