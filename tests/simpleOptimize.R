library(RLLVMCompile)

f =
function(x, y)
{
   tmp = 2 * x * y
   z = x + y
   x + tmp * z
}

m = Module()
#fc = compileFunction(f, DoubleType, list(DoubleType, DoubleType), module = m)

f =
function(n)
{
    total = 0L
    for(i in 1:n)
        total = total + i
    total
}

fc = compileFunction(f, Int32Type, list(Int32Type), module = m)



if(TRUE) {
mgr = passManager(NULL, FALSE)
passes = list( createPromoteMemoryToRegisterPass )
invisible(lapply(passes, function(f)  addPass(mgr, f())))

print(showModule(m))
run(mgr, m)
}

print(showModule(m))


