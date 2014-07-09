library(RLLVMCompile)

f = function(n) {
   ctr = 0L
   x = 0L
   for(i in 1:n) {
      printf("i = %d\n", i)
      x = 0L
      for(j in 1:i) {
         x = x + 1L
         ctr = ctr + 1L
      }
      x = 10L
   }
   ctr
}

fc = compileFunction(f, Int32Type, list(Int32Type))

stopifnot(.llvm(fc, 10) == 55)
