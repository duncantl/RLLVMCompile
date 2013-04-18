library(RLLVMCompile)
f =
function()
{
  1
}

mod = Module("foo")
fun = compileFunction(f, VoidType, mod = mod, name = "f")
.llvm(fun)
