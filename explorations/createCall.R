library(RLLVMCompile)

# create the call foo(1)
f =
function()
{
   zz = Rf_allocVector(6L, 2L)  # LANGSXP
   SETCAR(zz, Rf_install("foo"))
   cur = CDR(zz)
   SETCAR(cur, Rf_ScalarInteger(1L))
}

fc = compileFunction(f, VoidType, list())

