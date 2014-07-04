library(RLLVMCompile)

f =
function(x)
{
  ctr = 0
  for(i in x)
      ctr = ctr + i
  
  ctr
}

fc = compileFunction(f, DoubleType, list(REALSXPType))
.llvm(fc, as.numeric(1:10))

.llvm(fc, c(1, 2, 3))

