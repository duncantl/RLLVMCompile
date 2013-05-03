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

fc = compileFunction(f, Int32Type, list(Int32Type))

library(compiler)
fcc = cmpfun(f)

n = 1e4
tm.1e4 = rbind(r = system.time(f(n)),
               byte = system.time(fcc(n)),
               ll = system.time(.llvm(fc, n)))[, 1:3]


apply(tm.1e4, 2, function(x) x/min(x))
# OS X Apr 28 2013.
#     user.self sys.self   elapsed
#r    244.20388      Inf 244.97087
#byte  60.83495      Inf  61.16505
#ll     1.00000      NaN   1.00000


g = function(n1, n2) {
   ctr = 0L
   for(i in 1:n1)
      for(j in 1:n2) {
         ctr = ctr + 1L
      }
   ctr
}
