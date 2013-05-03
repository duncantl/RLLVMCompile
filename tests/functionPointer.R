library(RLLVMCompile)

f = function(x) x + 1L
f = function(x) 10L * x 

mod = Module()
ee = ExecutionEngine(mod)
fc = compileFunction(f, Int32Type, Int32Type, module = mod)
.llvm(fc, 2, .ee = ee)

fn = getPointerToFunction(fc, ee)

if(!file.exists("functionPointer.so"))
  system("R CMD SHLIB functionPointer.c")

dyn.load("functionPointer.so")
  # manually written
.Call("R_runWorker", 1:10, getNativeSymbolInfo("simpleWorker")$address)
.Call("R_runWorker", 1:10, fn@ref)


