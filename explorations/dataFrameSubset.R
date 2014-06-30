library(RLLVMCompile)

f =
function(l, i)
{
   l[i, 2]
}

# 
#dfType = DataFrameType(list(a = SEXPType, b = SEXPType)
#dfType = DataFrameType(list(a = getSEXPType("INT", TRUE), b = getSEXPType("REAL", TRUE)))
dfType = DataFrameType(list(a = INTSXPType, b = REALSXPType))
m = Module()
fc = compileFunction(f, DoubleType, list(dfType, Int32Type), module = m)
if(FALSE) {
x = data.frame(a = 1:10, b = 1:10 + 25.5)
.llvm(fc, x, 1L)
}

    
