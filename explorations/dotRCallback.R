library(RLLVMCompile)

a = 1L
foo =
function(i, j)
{
   cat("i =", i, ", class(j) =", class(j), "\n")
   10L
}

cb =
function(x)
{
   ans = .R(foo(a + 10L, list(a = 1, b = 1:10)), list(Int32Type, list(Int32Type, SEXPType)))
   ans + x
}

m = Module()
ee = ExecutionEngine(m)
fc = compileFunction(cb, Int32Type, list(Int32Type), module = m, .ee = ee, .fixIfAssign = FALSE)


.llvm(fc, 2L, .ee = ee)
