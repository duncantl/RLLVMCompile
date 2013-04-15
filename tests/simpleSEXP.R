library(RLLVMCompile)
f = function(x) { x }
ty = Rllvm:::getSEXPType("REAL")
fc = compileFunction(f, ty, list(ty), name = "f")

x = rnorm(10)
y = .llvm(fc, x)
stopifnot(identical(x, y))




