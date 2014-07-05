library(Rllvm)
load("ir.rda")
mm = parseIR( I( ir ) )
funs = getModuleFunctions(mm)

isDeclaredFunction =
function(f)
{
   length(getBlocks(f)) == 0L
}

isDecl = sapply(funs, isDeclaredFunction)
lapply(names(funs)[isDecl], llvmAddSymbol)
 

.llvm(mm$f, 3)


# Now re-load the native symbols.
# How do we determine which Functions are defined in the module and which are external.
#  Can we put meta-data in the module to identify
#
# The module knows which are defined and which are declared.
#
