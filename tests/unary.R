library(RLLVMCompile)

f =
function(x)
  - x

fc = compileFunction(f, Int32Type, Int32Type)
.llvm(fc, 20)


g =
function()
  - 9
gc = compileFunction(g, Int32Type)
.llvm(gc)

h =
function()
  +2
hc = compileFunction(h, Int32Type)
.llvm(hc)



