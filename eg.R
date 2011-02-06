library(Rllvm)
InitializeNativeTarget()

#mod <- Module('ctr')
source("examples/total.R")

a = compileFunction(Sum2, DoubleType, list(Int32Type))
verifyModule(a$mod)
ee = ExecutionEngine(a$mod)
Optimize(a$mod, ee)

run(a$fun, 1000, .ee = ee)

r = system.time(replicate(100, Sum1(1000)))
ll = system.time(replicate(100, run(a$fun, 1000, .ee = ee)))
