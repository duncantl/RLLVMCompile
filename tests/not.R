library(RLLVMCompile)
h = function(l) !l
hun = compileFunction(h, Int1Type, Int1Type)

stopifnot(.llvm(hun, TRUE) == FALSE)
stopifnot(.llvm(hun, FALSE) == TRUE)

# the following checks casting of the return value - from Int1Type to Int32Type
hun = compileFunction(h, Int32Type, Int32Type)
.llvm(hun, 1)
.llvm(hun, 0)  # -1 !!XXX  Now fixed so that we use a ZExt when we have an Int1Type.
               #  This is in our compiler, not Rllvm.


