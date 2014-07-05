library(RLLVMCompile)

f =
function(x)
{
  x[1] = 3.1415
  x[1]
}

fc = compileFunction(f, DoubleType, pointerType(DoubleType))
.llvm(fc, 3)

m = as(fc, "Module")
ir = showModule(m, TRUE)
save(ir, file = "ir.rda")


