library(RLLVMCompile)

f =
function(x)
{
   if(x < 10 && x > 5)
       20L
   else
       -1L
}

fc = compileFunction(f, Int32Type, list(DoubleType))
.llvm(fc, 7) # 20L
.llvm(fc, 12) # -1L
