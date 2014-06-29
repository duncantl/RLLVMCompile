library(RLLVMCompile)

cb =
function()
{
  ans = foo(1, 2) # a = 1, b = 2)
  printf("answer from R: %d\n", ans)
}


m = Module()
fc = compileFunction(cb, VoidType, list(), module = m,
                     .CallableRFunctions = list(foo = list(Int32Type, list())),
                     .ee = TRUE)


#m = as(fc, "Module")
ee = ExecutionEngine(m)
.llvm(m$setCall_foo_expression, quote(foo()), .ee = ee)

foo =
function(x = 1, a = TRUE)
{
   cat("In foo: x =", x, "a =", a, "\n")
   as.integer(x)
}

.llvm(fc, .ee = ee)

# We can change the expression and add more arguments, refer to variables in R's global environment.
.llvm(m$setCall_foo_expression, quote(foo(pi, FALSE)), .ee = ee)
.llvm(fc, .ee = ee)

m$create_foo_expression
.llvm(m$create_foo_expression, .ee = ee)
