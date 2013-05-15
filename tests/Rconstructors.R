library(RLLVMCompile)
if(FALSE) {
  # Need to be able to interpret whether integer() means
  # a SEXP type or just an Int32Type. Depends on  the context
  # If we knew x was the return value, we could infer the type
  # from the return type of the function.
g = function() { x = integer(); x}
fun = compileFunction(g, Int32Type)

g = function() { x = logical(); x}
fun = compileFunction(g, Int1Type)
.llvm(fun)
}

