library(RLLVMCompile)
INTSXPType = getSEXPType("INT")
REALSXPType = getSEXPType("REAL")

f = function(x) { x[2] + 10L }
# f = function(x) { x[2] + 10 } #XXX doesn't work. Need to cast x[2] up to double. But probably should cast 10 down. Certainly when we know it is a literal!
fc = compileFunction(f, VoidType, list(INTSXPType))
.llvm(fc, 1:3)   # no return value!

fc = compileFunction(f, Int32Type, list(INTSXPType))
.llvm(fc, 1:3)
