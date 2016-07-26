library(RLLVMCompile)

f =
function(x, i)
{
  x[i, 2L] == 1L
}

m = Module()
fc = compileFunction(f, Int1Type, list(RMatrixType(Int32Type), Int32Type), m)
.llvm(fc, matrix(1:10, 5, 2)[, 2:1], 1)

# Fails - comparing double to Int32Type
m = Module()
gc = compileFunction(f, Int1Type, list(RMatrixType(DoubleType), Int32Type), m)
.llvm(gc, matrix(1:10 + .1, 5, 2), 1)


