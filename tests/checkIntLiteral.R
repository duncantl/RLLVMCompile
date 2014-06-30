library(RLLVMCompile)

f =
function()
{
  Rf_ScalarInteger(1)
}

fc = compileFunction(f, SEXPType, list())
.llvm(fc)
