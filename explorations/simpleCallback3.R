library(RLLVMCompile)

# This is the clean version of the callback examples that requires the user do the least.
# The others expose setting the callback expressions.


# This illustrates how a function we compile into LLVM native code
# can call back to an R function.
# Here cb is the function we compile; foo() is the R function
# that we call back to.

# When we compile cb, we tell the compiler that foo() is an R function
# and we give it foo()'s signature.

#XXX We can make compileFunction() handle variable arguments to foo() and we would provide the
# signature for the call with the most parameters, or actually the union of the arguments
# across the calls.

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

ee = ExecutionEngine(m)

.llvm(fc, 10L, .ee = ee)




