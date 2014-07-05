library(RLLVMCompile)

f =
function(x, i)
{
    x[2, i]
}

fc = compileFunction(f, DoubleType, list(MatrixType(DoubleType), Int32Type))
m = as(fc, "Module")
.llvm(fc, matrix(1:20 + .1, 5, 4), 3)


g =
function(x)
{
    x[2]
}


gc = compileFunction(g, DoubleType, list(REALSXPType))
.llvm(gc, c(1, 12, 3))
