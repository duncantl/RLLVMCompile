library(RLLVMCompile)

m = Module()

ptrNULL = getNULLPointer(SEXPType)

gv = createGlobalVariable("Rcall", m, SEXPType, ptrNULL) # , linkage = InternalLinkage)

f =
function(tmp)
{
    Rcall = tmp
    R_PreserveObject(Rcall)
    Rf_PrintValue(Rcall)
}

fc = compileFunction(f, VoidType, list(SEXPType), module = m)

g = function()
{
  Rf_eval(Rcall, R_GlobalEnv)
}

llvmAddSymbol(R_GlobalEnv = getNativeSymbolInfo("R_GlobalEnv")) # $address)
gv = createGlobalVariable("R_GlobalEnv", m, SEXPType)

gc = compileFunction(g, VoidType, list(), module = m)

foo = function(i, predicate = FALSE) {
#  browser()  to show can still interact with R as usual.
  cat("In foo: ", i, "  ", predicate, "\n")
  i + 1L
}

if(FALSE) {
ee = ExecutionEngine(m)
.llvm(fc, quote(foo(1, TRUE)), .ee = ee)

.llvm(gc, .ee = ee)
}
