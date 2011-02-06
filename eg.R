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

B = 10
n = c(100, 1e3, 1e4, 1e5, 1e6, 1e7)
rr = sapply(n,
             function(n) 
               system.time(replicate(B, Sum2(n))))

ll = sapply(n,
             function(n) 
               system.time(replicate(B, run(a$fun, n, .ee = ee))))

plot(n, rr[3,]/ll[3,], type = 'l')
rr[3,]/ll[3,]
# [1]   0.0000   1.0000   9.4000  83.0000 317.0625 462.0935
