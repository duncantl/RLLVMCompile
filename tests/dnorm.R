library(RLLVMCompile)

f = function(x, mu = 0, sd = 1)
         1/sqrt(2 * pi * sd^2) * exp( - .5*( (x-mu)/sd ) ^2)

x = rnorm(5)
all(dnorm(x) == f(x))

fc = compileFunction(f, DoubleType, list(DoubleType, DoubleType, DoubleType))
fc = compileFunction(f, DoubleType, list(x = DoubleType, DoubleType, DoubleType),
                       vectorize = "x")

fc = compileFunction(f, arrayType(DoubleType), list(arrayType(DoubleType), DoubleType, DoubleType))

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
