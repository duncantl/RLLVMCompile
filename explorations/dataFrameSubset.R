library(RLLVMCompile)

f =
function(l, i)
{
   el = l[[i]]
    length(el)
}

dfType = dataFrameType(list(a = SEXPType, b = SEXPType)
fc = compileFunction(f, Int32Type, list(dfType, Int32Type))
if(FALSE) {
x = list(a = 1:10, b = rnorm(20))
.llvm(fc, x, 1L)
}

    
