f =
function(x)
{
   x == "bob"
}

library(RLLVMCompile)
trace(llvmAddSymbol)
llvmAddSymbol("strcmp")
fc = compileFunction(f, Int32Type, list(StringType))

showModule(fc)
#
#.llvm(fc, "jane")
#ee = ExecutionEngine(as(fc, "Module"))
#.llvm(fc, "jane", .ee = ee)
