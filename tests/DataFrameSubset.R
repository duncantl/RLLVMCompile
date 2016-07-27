library(RLLVMCompile)

f =
function(x, i)
{
  x[i, 2L]
}

m = Module()
# Use the SEXP type for the columns.
# What about factor. And LIST
#NOT:  fc = compileFunction(f, Int32Type, list(DataFrameType(list(DoubleType, Int32Type, StringType)), Int32Type), m)
fc = compileFunction(f, Int32Type, list(DataFrameType(list(REALSXPType, INTSXPType, STRSXPType)), Int32Type), m)
showModule(m)
d = data.frame(a = rnorm(10), b = 1:10, c = letters[1:10])
stopifnot(.llvm(fc, d, 3) == d[3, 2])
stopifnot(.llvm(fc, d, 10)  == d[10, 2])


f =
function(x, i, j)
{
  x[i, j]
}

fc = compileFunction(f, DoubleType, list(DataFrameType(list(REALSXPType, REALSXPType, REALSXPType)), Int32Type, Int32Type))
showModule(fc)
d = data.frame(a = as.numeric(1:10), b = as.numeric(101:110), c = rnorm(10))
stopifnot(.llvm(fc, d, 3, 1) == d[3, 1])
stopifnot(.llvm(fc, d, 10, 3)  == d[10, 3])

