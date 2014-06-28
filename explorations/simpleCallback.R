library(RLLVMCompile)

cb =
function()
{
  ans = foo() # a = 1, b = 2)
  printf("answer from R: %d\n", ans)
}

# Make a connection type class.

fc = compileFunction(cb, VoidType, list(),
                     .CallableRFunctions = list(foo = list(Int32Type, list())))

