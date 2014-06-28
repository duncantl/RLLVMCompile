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
i = .llvm(fc, 10)
i
class(i)


library(compiler)
nfc = cmpfun(f)

if(FALSE) {
a = system.time(f(1e7))
b = system.time(nfc(1e7))
ee = ExecutionEngine(fc)
.llvm(fc, 10, .ee = ee)
c = system.time(.llvm(fc, 1e7, .ee = ee))
a/b
b/c
a/c
info = sessionInfo()
save(a,b,c, info, file = sprintf("whileSpeed_%s.rda", Sys.info()["sysname"]))
}

# This version shows that we don't need the integer types on ctr and 1 in the loop.
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
