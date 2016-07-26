library(RLLVMCompile)

f =
function(x, i)
{
  x[i, 2L]
}

m = Module()
fc = compileFunction(f, StringType, list(RMatrixType(StringType), Int32Type), m)
showModule(m)
.llvm(fc, matrix(letters, 13, 2), 3)  # "p"

m = Module()
dc = compileFunction(f, DoubleType, list(RMatrixType(DoubleType), Int32Type), m)
showModule(m)
.llvm(dc, matrix(1:26 + .01, 13, 2), 3)  # "p"


f =
function(x, i)
{
  x[i, 5L]
}

m = Module()
gc = compileFunction(f, Int32Type, list(RMatrixType(Int32Type), Int32Type), m)
showModule(m)

.llvm(gc, matrix(1:26, 13, 2), 3)  # "p"


f =
function(x, i, j)
{
  x[i, j]
}

m = Module()
gc = compileFunction(f, Int32Type, list(RMatrixType(Int32Type), Int32Type, Int32Type), m)
showModule(m)


