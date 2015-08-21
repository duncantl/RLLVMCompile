library(RLLVMCompile)

f =
function(i)
{
   i - 1
}

fc = compileFunction(f, Int32Type, Int32Type)
.llvm(fc, 2)


g =
function(x, i)
{
   x[ i - 1 ]
}
gc = compileFunction(g, Int32Type, list(pointerType(Int32Type), Int32Type))
