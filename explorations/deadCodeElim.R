library(RLLVMCompile)

f =
function(x)
{
   a = 1L
   x + 1L
}

fc = compileFunction(f, Int32Type, list(Int32Type))
showModule(fc)


fc1 = compileFunction(f, Int32Type, list(Int32Type), optimize = FALSE)
m = as(fc1, "Module")
ee1 = ExecutionEngine(m)
# http://lunarglass.googlecode.com/svn/branches/3.1upgrade/TopToBottom.cpp
passManager = passManager(NULL, FALSE) # getPassManager(m, ee) # passManager(m)
#Doesn't work: passManager = getPassManager(m, ee) # passManager(m)
# This is the pass that kills of the assignment to a.
deadStore = createDeadStoreEliminationPass()
addPass(passManager, deadStore)
#deadCode = .Call("R_createDeadCodeEliminationPass")
#addPass(passManager, deadCode)
#adce = .Call("R_createAggressiveDCEPass")
#addPass(passManager, adce)

showModule(m)
run(passManager, m)
showModule(m)

Optimize(m, ee1, mgr = passManager)
showModule(m)

N = 1e7
ee = ExecutionEngine(as(fc, "Module"))
tm = system.time(replicate(N, .llvm(fc, 10L, .ee = ee)))
tm1 = system.time(replicate(N, .llvm(fc1, 10L, .ee = ee1)))

print(tm/tm1)

# N = 1e6 10%, 4%, 2.1%, 7.5%, 9.1%  improvement
# N = 1e5 3%
# N = 1e7 40%, 26%




