library(RLLVMCompile)
test = function(x) {
   1/x   # (x + 2)
}
testc = compileFunction(test, DoubleType, list(DoubleType))
.llvm(testc)
