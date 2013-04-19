library(RLLVMCompile)
fib =
function(n)
{
  if(n == 0L || n == 1L)
     n
  else
     fib(n - 1L) + fib(n - 2L)
}

fc = compileFunction(fib, Int32Type, list(n = Int32Type))

.llvm(fc, 4)

a = system.time(fib(30))
c = system.time(.llvm(fc, 30))

library(compiler)
ff = cmpfun(fib)
b = system.time(ff(30))

a/c  # on OS X 1000 times faster.

