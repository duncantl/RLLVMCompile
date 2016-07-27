library(RLLVMCompile)

f =
function(d)
{
    d == 2L
}

gc = compileFunction(f, Int1Type, list(Int32Type))
showModule(gc)

gc = compileFunction(f, Int1Type, list(DoubleType))
showModule(gc)
