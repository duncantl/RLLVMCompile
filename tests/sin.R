# Exploring using different intrnsic signatures.
# Problem with return Int32Type and so invalid module
#
# Update: Currently, error is "Can't reference function sin in module"

library(RLLVMCompile)

isin =
function(i)
{  
  sin(i)
}

if(FALSE) {
  f = compileFunction(isin, Int32Type, list(Int32Type), .insertReturn = TRUE)
  f = compileFunction(isin, FloatType, list(FloatType), .insertReturn = TRUE)
}
