library(RLLVMCompile)
f = function(x, y)  { if(x < y && x > y/2)  3 else 4}
gc = compileFunction(f, Int32Type, list(DoubleType, DoubleType))

stopifnot(.llvm(gc, 2, 3) == f(2, 3))
stopifnot(.llvm(gc, 2, 9) == f(2, 9))
