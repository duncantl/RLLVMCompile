library(Rllvm)
load("ir.rda")
mm = parseIR( I( ir ) )
funs = getModuleFunctions(mm)

.llvm(mm$f, 3)
