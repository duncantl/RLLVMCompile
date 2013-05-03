library(RLLVMCompile)
f = function(n)   numeric(n)  # okay.

f = function(n)  {
   ans = numeric(n)  # okay.
   ans
}   

vf = function(n)  { Rf_PrintValue( Rf_allocVector(14L, n) )}
g = function(n) {
    ans = Rf_allocVector(14L, n)  # numeric(n)
    Rf_protect(ans)
    memset(REAL(ans), 0L, n * 8L)
    Rf_PrintValue(ans)
    Rf_unprotect(1L)
#    ans
  }

vf2 = function(n)  {
   ans = R_my_allocVectorInit()
   ans
}

f = function(n) {
   R_NEW_NUMERIC(n)
}
mod = Module()
declareFunction(list(pointerType(Int8Type), pointerType(Int8Type), Int32Type, Int32Type), "memset", mod)
declareFunction(list(REALSXPType), "R_my_allocVector", mod)
declareFunction(list(REALSXPType), "R_my_allocVectorInit", mod)

declareFunction(list(REALSXPType, Int32Type), "R_NEW_NUMERIC", mod)

dyn.load("testAlloc.so")
llvmAddSymbol("R_my_allocVector", "R_my_allocVectorInit", "R_NEW_NUMERIC")

# fc = compileFunction(vf2, REALSXPType, Int32Type, module = mod)
fc = compileFunction(g, REALSXPType, Int32Type, module = mod)
#fc = compileFunction(vf, VoidType, Int32Type, module = mod)
#.llvm(fc, 1L)
#fc = compileFunction(f, REALSXPType, Int32Type)
