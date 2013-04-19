library(RLLVMCompile)
readTo =
function()
{
   tmp = character()
   1L
}
fun = compileFunction(readTo, Int32Type, optimize = FALSE)
showModule(fun)
.llvm(fun)


