library(RLLVMCompile)
f = function(x) x + 1L
mod = Module()
fc = compileFunction(f, Int32Type, Int32Type, module = mod)
g = function(x) sapply(x, f)
gc = compileFunction(g, getSEXPType("INT"), getSEXPType("INT"), module = mod)

.llvm(fc, 1)
.llvm(gc, 1:10)

ee = ExecutionEngine(mod)
tm = system.time(replicate(1e5, .llvm(fc, 1L)))
tm = system.time(replicate(1e5, .llvm(fc, 1L, .ee = ee)))


