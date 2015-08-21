library(RLLVMCompile)

isum =
function(x, n)
{
  total = 0L
  for(i in 1:n)
      total = total + x[i]
  total
}

m = Module()
compileFunction(isum, Int32Type, list(pointerType(Int32Type), Int32Type), module = m)

f =
function(x, nr, nc)  # have the compiler add nc
{
  ans = integer(nc)
  for(i in 1:nc)
      ans[i] = isum(x[, i], nr)

  ans
}

fc = compileFunction(f, SEXPType, list(new("MatrixType", elType = Int32Type), Int32Type, Int32Type), module = m)
# pointerType(Int32Type)

mat = matrix(1:15, 3, 5)
.llvm(fc, mat, nrow(mat), ncol(mat))

