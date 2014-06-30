library(RLLVMCompile)

foo =
function(x, y = 1, z = TRUE)
{
browser()    
  print(y)  # this will be the value of bob
   10L
}

cb =
function()
{
  ctr = 20L
  ans = foo(bob, x = ctr, structure(1:10, class = "Jane", names = letters[1:10]))
}


m = Module()
ee = ExecutionEngine(m)

# Do this with .ee = NULL and .ee = ee.
# These two are quite different.
fc = compileFunction(cb, VoidType, list(), module = m,
                     .CallableRFunctions = list(foo = list(Int32Type, list(Int32Type, DoubleType, Int32Type))),
                     .RGlobalVariables = "bob", .ee = NULL)


bob = 10
.llvm(fc, .ee = ee)




