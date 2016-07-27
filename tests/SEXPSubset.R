library(RLLVMCompile)

# Simple test that returns nothing
f = function(x) { x[2L] = 10L }
fc = compileFunction(f, VoidType, list(INTSXPType))
.llvm(fc, 1:3, .all = TRUE)


f =
function(x)
{
   x[1] = 20 # don't need the L. Check if casting is automatic
   x
}
fc = compileFunction(f, INTSXPType, list(INTSXPType))
.llvm(fc, 1:3) # , .all = TRUE)

###### REAL()

f = function(x) { x[2L] = 10.2 }
fc = compileFunction(f, VoidType, list(REALSXPType))
.llvm(fc, c(1, 2, 3))

f =
function(x)
{
   x[1] = 20.3 # 20 should be able to cast
   x
}
fc = compileFunction(f, REALSXPType, list(REALSXPType))
.llvm(fc, c(1, 2, 3))

##

