library(RLLVMCompile)

foo =
function(d = 1, i = 1L, l = TRUE, s = "string", any = NULL, other = 2)
{
   cat("In foo: d =", d, "i =", i, "l =", l, "s = ", s, "class(any) =", class(any), "other =", other, "\n")
   as.integer(2*i)
}

cb =
function(d1, i1, l1, s1, any1)
{
  printf("input to cb: %d\n", i1)    
  ans = foo(d1, i1, l1, s1, any1)
  printf("answer from R: %d\n", ans)
}


m = Module()
fc = compileFunction(cb, VoidType, list(DoubleType, Int32Type, Int32Type, StringType, SEXPType), module = m,
                     .CallableRFunctions = list(foo = list(Int32Type, list(DoubleType, Int32Type, Int32Type, StringType, SEXPType))))


ee = ExecutionEngine(m)

.llvm(fc, pi, 10L, FALSE, "our own string", 1:2, .ee = ee)
.llvm(fc, pi, 10L, FALSE, "our own string", plot, .ee = ee)
.llvm(fc, pi, 10L, FALSE, "our own string", mtcars, .ee = ee)




