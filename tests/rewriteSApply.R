library(RLLVMCompile)
library(Rllvm) # needed for the INTSXPType, etc.

z = RLLVMCompile:::rewriteSApply(quote(sapply(x, f)), REALSXPType, Int32Type)

z = RLLVMCompile:::rewriteSApply(quote(sapply(x, f)), INTSXPType, DoubleType)
  # Additional arguments in each call to FUN.
z = RLLVMCompile:::rewriteSApply(quote(sapply(x, f, 1, 2)), INTSXPType, DoubleType)


z = RLLVMCompile:::rewriteSApply(quote(sapply(x, f)), REALSXPType, DoubleType)

z = RLLVMCompile:::rewriteSApply(quote(sapply(x, f)), REALSXPType, StringType)
z = RLLVMCompile:::rewriteSApply(quote(sapply(x, f, 1, 2)), REALSXPType, StringType)

z = RLLVMCompile:::rewriteSApply(quote(sapply(x, f)), REALSXPType, SEXPType)

z = RLLVMCompile:::rewriteSApply(quote(sapply(x, f)), VECSXPType, SEXPType)


#XXX  note that the CHARSXP is being passed directly to f which expects a StringType
z = RLLVMCompile:::rewriteSApply(quote(sapply(x, f)), STRSXPType, SEXPType)
