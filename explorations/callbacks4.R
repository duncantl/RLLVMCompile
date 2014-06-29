library(RLLVMCompile)

foo =
function(i, j)
{
   cat("i =", i, ", class(j) =", class(j), "\n")
   10L
}

cb =
function(a)
{
  ans = foo(a + 10L, list(a = 1, b = 1:10))
}


m = Module()
ee = ExecutionEngine(m)
fc = compileFunction(cb, VoidType, list(Int32Type), module = m,
                     .CallableRFunctions = list(foo = list(Int32Type, list(Int32Type, SEXPType))),
                     .ee = ee)


.llvm(fc, 2L, .ee = ee)




