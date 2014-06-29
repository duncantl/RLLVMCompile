library(RLLVMCompile)

foo =
function(x, y = 1, z = TRUE)
{
   10L
}

cb =
function()
{
  ctr = 20L
  ans = foo(bob, x = ctr, 1L)
}


m = Module()
fc = compileFunction(cb, VoidType, list(), module = m,
                     .CallableRFunctions = list(foo = list(Int32Type, list(Int32Type, DoubleType, Int32Type))))

ee = ExecutionEngine(m)

bob = 10
.llvm(fc, .ee = ee)




