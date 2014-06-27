library(RLLVMCompile)

f =
function(l, i)
{
   el = l[[i]]
   length(el)
}

fc = compileFunction(f, Int32Type, list(SEXPType, Int32Type))
if(FALSE) {
x = list(a = 1:10, b = rnorm(20))
.llvm(fc, x, 1L)
.llvm(fc, x, 2L)
}

    
