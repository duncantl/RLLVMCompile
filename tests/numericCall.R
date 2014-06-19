library(RLLVMCompile)
f = function(n) {  x = numeric(n) ; x[1] = 1; x}
fc = compileFunction(f, REALSXPType, Int32Type)
# ??? Does this work  showModule(fc)
#  Do we need to protect the new vector?
#  Should we have a GEP?
.llvm(fc, 10)


