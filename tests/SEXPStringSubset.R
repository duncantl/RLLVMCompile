library(RLLVMCompile)
STRSXPType = getSEXPType("STR")

f = function(x, a) { x[1] = a}
fc = compileFunction(f, VoidType, list(STRSXPType, STRSXPType))




