library(RLLVMCompile)

f = function(n) {
   ctr = 0L
   for(i in 1:n)
      for(j in 1:i) {
#         print(c(i, j))
         ctr = ctr + 1L
      }
   ctr
}

fc = compileFunction(f, Int32Type, Int32Type)

stopifnot(.llvm(fc, 10) == 55L)
cat("okay\n")

if(FALSE) {
library(compiler)
fcc = cmpfun(f)

n = 1e4
ee = ExecutionEngine(as(fc, "Module"))
print(.llvm(fc, n, .ee = ee))

tm.1e4 = rbind(r = system.time(f(n)),
               byte = system.time(fcc(n)),
               ll = system.time(.llvm(fc, n, .ee = ee)))[, 1:3]


apply(tm.1e4, 2, function(x) x/min(x))
# OS X Apr 28 2013.
#     user.self sys.self   elapsed
#r    244.20388      Inf 244.97087
#byte  60.83495      Inf  61.16505
#ll     1.00000      NaN   1.00000
#Linux Apr 28, 2013
#     user.self sys.self   elapsed
#r     283.6190      NaN 283.66667
#byte   61.2619      Inf  61.34921
#ll      1.0000      NaN   1.00000

# OS X much faster machine
# r = 102.68675  byte = 26.73494  ll = 1.00000 
#        r      byte        ll 
#     109.08235  29.34118   1.00000

g = function(n1, n2) {
   ctr = 0L
   for(i in 1:n1)
      for(j in 1:n2) {
         ctr = ctr + 1L
      }
   ctr
}
}
