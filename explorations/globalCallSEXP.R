library(RLLVMCompile)

# Low-level - manual - callback to R
#
# Create a global variable to hold the R call/expression, and a routine to set it from R,
# Then a routine to evaluate that call.
#
# We need R_GlobalEnv so we create a non-local variable  in the module and then
# load the symbol into LLVM with llvmAddSymbol()


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
  x = Rf_eval(Rcall, R_GlobalEnv)
  Rf_PrintValue(x)
  i = Rf_asInteger(x)
  printf("LLVM: R result = %d\n", i)
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
.llvm(fc, quote(foo(1L, TRUE)), .ee = ee)

.llvm(gc, .ee = ee)
}
