library(RLLVMCompile)

Dnorm = function(x, mu = 0, sd = 1)
         1/sqrt(2 * pi * sd^2) * exp( - .5*( (x-mu)/sd ) ^2)

x = rnorm(5)
all(dnorm(x) == Dnorm(x))

 # Fails on linux with a mismatch of arguments in one of the calls.
fc = compileFunction(Dnorm, DoubleType, list(DoubleType, DoubleType, DoubleType))
if(FALSE) {
fc = compileFunction(f, DoubleType, list(x = DoubleType, DoubleType, DoubleType),
                       vectorize = "x")
fc = compileFunction(f, arrayType(DoubleType), list(arrayType(DoubleType), DoubleType, DoubleType))
}

mod = as(fc, "Module")
g = vectorizeFunction(Dnorm, scalar = "Dnorm")
gc = compileFunction(g, REALSXPType, list(REALSXPType, DoubleType, DoubleType), module = mod)

all(.llvm(gc, x, 0, 1) == dnorm(x, 0, 1))

library(compiler)
Dnormc = cmpfun(Dnorm)

n = 1e5
x = rnorm(n)

if(FALSE) {
ee = ExecutionEngine(mod)
tm.1e5 = list(llvm = system.time(replicate(20, .llvm(gc, x, 0, 1, .ee = ee))),
              bytec = system.time(replicate(20, Dnormc(x))),
              native = system.time(replicate(20, dnorm(x))),
              r = system.time(replicate(20, g(x))))
}

#
# double *fc(double *x, size_t x_length, double mu, double sd)
# {
#   int i;
#   double *ans;
#   ans = malloc(sizeof(double) * x_length);
#   for(i = 0; i < x_length; i++)
#     ans[i] = expr(x[i], mu, sd);
#   return(ans);
# }
#  function(x)
#
#
