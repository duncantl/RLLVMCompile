library(RLLVMCompile)
f <- function(x) {
    s <- 0.0
    for (y in x)
        s <- s + y
    s
}

#fc = compileFunction(f, DoubleType, pointerType(DoubleType))
fc = compileFunction(f, DoubleType, REALSXPType)
x <- as.double(1 : 1e7)

ee = ExecutionEngine(fc)
.llvm(fc, x, .ee = ee)


a = system.time(.llvm(fc, x, .ee = ee))
b = system.time(f(x))

library(compiler)
fbc <- cmpfun(f)
c = system.time(fbc(x))

rbind(b/a, c/a)

d = system.time(sum(x))

