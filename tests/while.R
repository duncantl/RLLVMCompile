library(RLLVMCompile)
f =
function(to)
{
    ctr = 0L
    while(ctr <= to)
      ctr = ctr + 1L
    ctr
}

fc = compileFunction(f, Int32Type, list(Int32Type), name = "f")
class(.llvm(fc, 10))


library(compiler)
nfc = cmpfun(f)

a = system.time(f(1e6))
b = system.time(nfc(1e6))
c = system.time(.llvm(fc, 1e6))
info = sessionInfo()
save(a,b,c, info, file = "whileSpeed_OSX.rda")

# This version shows that we don't need the integer types
# 
g =
function(to)
{
    ctr = 0
    while(ctr <= to)
      ctr = ctr + 1
    ctr
}
gc = compileFunction(f, Int32Type, list(Int32Type), name = "g")
class(.llvm(gc, 10))
