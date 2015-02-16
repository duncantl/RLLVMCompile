library(RLLVMCompile)

f =
function(x)
{
  ctr = 0
  n = length(x)
  ans = numeric(n)
  for(i in 1:n) {
      printf("%d\n", i)
      ans[i] = x[i] + ctr
      ctr = ctr + 1L
  }
  
  ans
}

fc = compileFunction(f, REALSXPType, list(REALSXPType))
.llvm(fc, as.numeric(1:10))

.llvm(fc, c(1, 2, 3))

