library(RLLVMCompile)
INTSXPType = getSEXPType("INT")
STRSXPType = getSEXPType("STR")

mod = Module()
FILEType = pointerType(Int8Type)
declareFunction(list(StringType, Int32Type, FILEType), "readTo", mod)
f = function(lineSkip, file) sapply(lineSkip, readTo, file)
fc = compileFunction(f, STRSXPType, list(INTSXPType, FILEType), module = mod)

