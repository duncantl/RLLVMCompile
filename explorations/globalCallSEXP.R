library(RLLVMCompile)

m = Module()

ptrNULL = .Call("R_ConstantPointerNull_get", SEXPType, PACKAGE = "Rllvm")

gv = createGlobalVariable("Rcall", m, SEXPType, ptrNULL) # , linkage = InternalLinkage)

f =
function(tmp)
{
    Rcall = tmp
    R_PreserveObject(Rcall)
    Rf_PrintValue(Rcall)
}

fc = compileFunction(f, VoidType, list(SEXPType), module = m)

g = function(env)
{
  Rf_eval(Rcall, env)
}

llvmAddSymbol(R_GlobalEnv = getNativeSymbolInfo("R_GlobalEnv"))
#gv = createGlobalVariable("R_GlobalEnv", m, SEXPType)

gc = compileFunction(g, VoidType, list(SEXPType), module = m)

foo = function(i, predicate = FALSE) {
  cat("In foo: ", i, "  ", predicate, "\n")
  i + 1L
}

if(FALSE) {
ee = ExecutionEngine(m)
.llvm(fc, quote(foo(1, TRUE)), .ee = ee)

.llvm(gc, globalenv(), .ee = ee)
}
