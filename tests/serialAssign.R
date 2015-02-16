library(RLLVMCompile)
f =
function(n)
{
   xpos = ypos = numeric(n)
   xpos
}

fc = compileFunction(f, REALSXPType, list(Int32Type))
showModule(fc)
