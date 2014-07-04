library(RLLVMCompile)

f =
function(x)
{
   ans = 0L
   z = x < 2L
   z
#       ans = 3L
}

fc = compileFunction(f, Int32Type, list(Int32Type))
showModule(fc)
