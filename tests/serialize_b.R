library(Rllvm)
load("ir.rda")
SEXPType # force this.
mm = parseIR( I( ir ) )
funs = getModuleFunctions(mm)

isDeclaredFunction =
function(f)
   length(getBlocks(f)) == 0L

isDecl = sapply(funs, isDeclaredFunction)
lapply(names(funs)[isDecl], llvmAddSymbol)

print(getType(mm$f[[1]])@ref)
.Call("R_Argument_mutateType", mm$f[[1]], SEXPType)
.Call("R_Argument_mutateType", mm$Rf_length[[1]], SEXPType)
.Call("R_Argument_mutateType", mm$REAL[[1]], SEXPType)
print(getType(mm$f[[1]])@ref)
print( sameType(getType(mm$f[[1]]), SEXPType))
print(SEXPType@ref)

showModule(mm)

# When we call this we still have the old reference to the SEXP type.
.llvm(mm$f, as.numeric(1:10))

