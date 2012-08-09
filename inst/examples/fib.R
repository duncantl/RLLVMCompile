fib =
function(n)
{
  if(n == 0 || n == 1)
     n
  else
     fib(n - 1) + fib(n - 2)
} 

if(FALSE) {
 library(RLLVMCompile)
 fc = compileFunction(fib, types = Int32Type, returnType = Int32Type)
}
