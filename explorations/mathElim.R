f =
function(x)
{
   n = 0L
   x + n
}
library(RLLVMCompile)
fc = compileFunction(f, Int32Type, list(Int32Type))
.llvm(fc, 10L)


library(CodeAnalysis)
f = findUnusedAssignments(f)

library(CodeDepends)


