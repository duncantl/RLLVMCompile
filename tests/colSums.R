library(RLLVMCompile)

isum =
function(x, n)
{
  total = 0
  for(i in 1:n)
      total = total + x[i]
  total
}

f =
function(x, nr, nc)  # have the compiler add nc
{
  ans = integer(nc)
  for(i in 1:nc)
      ans[i] = isum(x[, i], nr)

  ans
}

m = Module()
compileFunction(isum, Int32Type, list(pointerType(Int32Type), Int32Type), module = m)
fc = compileFunction(f, SEXPType, list(new("MatrixType", elType = Int32Type), Int32Type, Int32Type), module = m)


mat = matrix(1:15, 3, 5)
ans = .llvm(fc, mat, nrow(mat), ncol(mat))
stopifnot(all( ans == colSums(mat)))


# Compile for numeric matrices


isum =
function(x, n)
{
  total = 0
  for(i in 1:n)
      total = total + x[i]
  total
}

body(f)[[2]][[3]][[1]] = as.name("numeric")
m1 = Module()
compileFunction(isum, DoubleType, list(pointerType(DoubleType), Int32Type), module = m1, .localVarTypes = list(total = DoubleType))
fc = compileFunction(f, SEXPType, list(new("MatrixType", elType = DoubleType), Int32Type, Int32Type), module = m1)


mat = matrix(1:15+.5, 3, 5)
ans = .llvm(fc, mat, nrow(mat), ncol(mat))
stopifnot(all( ans == colSums(mat)))

