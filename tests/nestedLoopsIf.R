library(RLLVMCompile)

f = function(n) {
   ctr = 0L
   for(i in 1:n)
      for(j in 1:i) {
         if( i == j)
            printf("diagonal %d\n", i)
         ctr = ctr + 1L
      }
   ctr
}

fc = compileFunction(f, Int32Type, list(Int32Type))
stopifnot(.llvm(fc, 10L) == 55L)
cat("okay\n")




