library(RLLVMCompile)
dyn.load("testAlloc.so")
llvmAddSymbol("sayHi")
mod = Module()
declareFunction(list(VoidType), "sayHi", mod)

f = function() sayHi()
fc = compileFunction(f, VoidType, module = mod)

# .llvm(fc)

