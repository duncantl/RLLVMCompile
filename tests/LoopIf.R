library(RLLVMCompile)

# Checks an if statement inside a loop.
#
# Need to also test an if statement with a continue/next/break

f =
function(x, n)
{
  ctr = 0L
  for(i in 1:n) {
      if(x[i] > 2)
        ctr = ctr + 1L
  }
  ctr
}

f = compileFunction(f, Int32Type, list(INTSXPType, Int32Type))
#ee = ExecutionEngine(f)
set.seed(13125)
v = rpois(100, 4)
stopifnot(.llvm(f, v, length(v)) == sum(v > 2))
