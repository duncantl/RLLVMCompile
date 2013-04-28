library(RLLVMCompile)
STRSXPType = getSEXPType("STR")
SEXPType = getSEXPType()

#f = function(x, a) { x[1] = a}
#fc = compileFunction(f, VoidType, list(STRSXPType, STRSXPType))

f = function(x, a, i) { x[i] = a}
fc = compileFunction(f, VoidType, list(STRSXPType, STRSXPType, Int32Type))
.llvm(fc, letters, "duncan", 1L)

g = function(x, a, i) { x[i] = a; x}
gc = compileFunction(g, STRSXPType, list(STRSXPType, STRSXPType, Int32Type))
.llvm(gc, letters, "duncan", 1L)





