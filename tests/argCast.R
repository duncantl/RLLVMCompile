library(RLLVMCompile)

m = Module()
fib = Function("fib", Int32Type, Int32Type, module = m)

f =
function()
{
  fib(3)
}
fc = compileFunction(f, Int32Type,  module = m)
