library(RLLVMCompile)

# illustrate how to specify the type for a local variable.

isum =
function(x, n)
{
  total = 0
  for(i in 1:n)
      total = total + x[i]
  total
}

# This assumes total should be an integer. It doesn't look ahead to determine that total is the return value
# and know it is a DoubleType.
fc = compileFunction(isum, DoubleType, list(pointerType(DoubleType), Int32Type))

# We can turn off the interpretation of 0 as an integer when it is an intializing value with .integerLiterals.
# This applies to all cases in our compilation call.
fc = compileFunction(isum, DoubleType, list(pointerType(DoubleType), Int32Type), .integerLiterals = FALSE)

# So if we don't want to disable the integerLiterals for evey case, but need to specify the type for
# one or more local variables, we can specify these types via .localVarTypes.
fc = compileFunction(isum, DoubleType, list(pointerType(DoubleType), Int32Type), .localVarTypes = list(total = DoubleType))




