library(RLLVMCompile)
Dnorm = function(x, mu = 0, sd = 1)
                   1/sqrt(2 * pi * sd^2) * exp( - .5*( (x-mu)/sd ) ^2)

#Dnorm = function(x, mu = 0, sd = 1)
#                   x + 1

fc = compileFunction(Dnorm, DoubleType, list(DoubleType, DoubleType,  DoubleType))
#source('../R/makeVectorized.R')

g =  function(x, mu, sd) {
       n = length(x)
       ans = numeric(n)
       for(i in 1:n) 
           ans[i] = Dnorm(x[i], mu, sd)

       ans
   }

gc = compileFunction(g, REALSXPType, list(REALSXPType, DoubleType, DoubleType), modul = as(fc, "Module"))

.llvm(gc, c(-1, 0, 1), 0, 1)

stopifnot(all(.llvm(gc, c(-1, 0, 1), 0, 1) == dnorm(c(-1,0,1))))

v = vectorizeScalarFunction(Dnorm, fc)

