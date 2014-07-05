library(RLLVMCompile)
f =
function(x)
  x != NULL

m = Module()
fc = compileFunction(f, Int1Type, pointerType(DoubleType), module = m)

.llvm(fc, c(1, 2))
.llvm(fc, new("externalptr"))

# getNULLPointer(pointerType(DoubleType)))
