library(RLLVMCompile)

# This was written to test the compilation mechanism when there is a for loop
# followed by another for loop.  In compile.{ (aka compileExpressions),
# we were concluding the second loop by jumping back to the block following the
# first loop. Hence, we repeated the second loop ad naueseam.

f =
function(x)
{
   n = length(x)
   y = numeric(n)
   for(i in 1:n)
       y[i] = 0

   for(i in 1:n)
     for(j in 1:i)
        y[i] = x[j] + y[i]

   y
}

fc = compileFunction(f, REALSXPType, REALSXPType)

x = as.numeric(1:9)
stopifnot(identical(f(x), .llvm(fc, x)))
