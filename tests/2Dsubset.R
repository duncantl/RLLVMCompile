library(RLLVMCompile)


f =
function(x)
{
   x[2, 3] + 1
}

fc = compileFunction(f, DoubleType, list(x = new("MatrixType", elType = DoubleType)))

m = matrix(as.numeric(1:15), 3, 5)
.llvm(fc, m)
