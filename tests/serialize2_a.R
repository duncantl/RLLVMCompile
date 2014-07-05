library(RLLVMCompile)

f =
function(x)
{
   x + 2
}

fc = compileFunction(f, DoubleType, DoubleType)

m = as(fc, "Module")
.llvm(m$f, 3)
#createGlobalVariable("num", m, Int32Type, 10L)

ir = showModule(m, TRUE)

save(ir, file = "ir.rda")
rm(m)

#Rllvm::shutdown()
#InitializeNativeTarget()
##############


