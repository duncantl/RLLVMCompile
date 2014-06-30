library(RLLVMCompile)
f =
function(x)
{
  ctr = x + 1L
  ctr
}

fc = compileFunction(f, Int32Type, list(Int32Type))
.llvm(fc, 1)
