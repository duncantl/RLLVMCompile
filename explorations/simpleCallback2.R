library(RLLVMCompile)


foo =
function(x = 1L, a = TRUE)
{
   cat("In foo: x =", x, "a =", a, "\n")
   as.integer(2*x)
}

cb =
function(i)
{
  printf("input to cb: %d\n", i)    
  ans = foo(i, 2) # a = 1, b = 2)
  printf("answer from R: %d\n", ans)
}


m = Module()
fc = compileFunction(cb, VoidType, list(Int32Type), module = m,
                     .CallableRFunctions = list(foo = list(Int32Type, list(Int32Type))),
                     .ee = TRUE)


showModule(m)

ee = ExecutionEngine(m)
.llvm(m$setCall_foo_expression, quote(foo(i, "xyz")), .ee = ee)
.llvm(fc, 10L, .ee = ee)


# Now create the call with our routine.

.llvm(m$create_foo_expression, .ee = ee)
.llvm(fc, 10L, .ee = ee)

