library(RLLVMCompile)

isum =
function(x, n)
{
  total = 0
  for(i in 1:n)
      total = total + x[i]
  total
}

compileFunction(isum, DoubleType, list(pointerType(DoubleType), Int32Type), .localVarTypes = list(total = DoubleType))
