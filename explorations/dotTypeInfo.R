library(RLLVMCompile)

cb =
function(x, y)
{
     .typeInfo(DoubleType, Int32Type, DoubleType)
     x + y
}

m = Module()
ee = ExecutionEngine(m)
fc = compileFunction(cb, module = m, .ee = ee, .fixIfAssign = FALSE)


.llvm(fc, 2L, 3, .ee = ee)
