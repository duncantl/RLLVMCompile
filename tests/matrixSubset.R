library(RLLVMCompile)

f =
function(x, i)
{
  x[i, 2L]
}

m = Module()
fc = compileFunction(f, StringType, list(MatrixType(StringType), Int32Type), m)
.llvm(fc, matrix(letters, 13, 2), 3)  # "p"

m = Module()
dc = compileFunction(f, DoubleType, list(MatrixType(DoubleType), Int32Type), m)
.llvm(dc, matrix(1:26 + .01, 13, 2), 3)  # "p"



m = Module()
gc = compileFunction(f, Int32Type, list(MatrixType(Int32Type), Int32Type), m)
.llvm(gc, matrix(1:26, 13, 2), 3)  # "p"


