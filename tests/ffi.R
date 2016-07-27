library(RLLVMCompile)

f =
function(x)
{
  ctr = 0
  for(i in x)
      ctr = ctr + i
  
  ctr
}

fc = compileFunction(f, DoubleType, list(REALSXPType))

ee = ExecutionEngine(fc)
finalizeEngine(ee)
funptr = getPointerToFunction(fc, ee)

library(Rffi)
sig = CIF(doubleType, list(sexpType))

x = as.numeric(1:10)
ans = callCIF(sig, funptr@ref, x)


