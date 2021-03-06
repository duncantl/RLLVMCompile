library(RLLVMCompile)

#
# What does this show?
# It works, but what does it show?
#

f =
function(m, i)
{
  m[i, 1L] = 10
  m[1, 2L] = 100
}
library(RLLVMCompile)
numericMatrixType = MatrixType(DoubleType) # REALSXPType # # no dimensions known at compile time.
fc = compileFunction(f, VoidType, list(numericMatrixType, Int32Type))

m = matrix(0, 3, 3)
ans = .llvm(fc, m, 2, .all = TRUE)
print(m)
