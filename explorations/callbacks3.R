library(RLLVMCompile)

foo =
function(i)
{
   cat("i =", i, "\n")
   10L
}

cb =
function(a)
{
  ans = foo(a + 10L)
}


m = Module()
fc = compileFunction(cb, VoidType, list(Int32Type), module = m,
                     .CallableRFunctions = list(foo = list(Int32Type, list(Int32Type))))

ee = ExecutionEngine(m)

.llvm(fc, 2L, .ee = ee)




