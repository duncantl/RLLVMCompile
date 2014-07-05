library(RLLVMCompile)
f =
function(x)
  !x

fc = compileFunction(f, Int32Type, Int32Type)

.llvm(fc, 0)
.llvm(fc, 1)





