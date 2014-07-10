library(RLLVMCompile)

f =
function(n)
{
   ctr = 0L
   for(i in 1:n) {
       while(TRUE) {
           ctr = ctr + 1L
           if(ctr == 3L + i) #XXX try with 3 + i and have the compiler figure out what we meant.
               break
       }
   }
   ctr
}

fc = compileFunction(f, Int32Type, Int32Type)
stopifnot(.llvm(fc, 4) == 7)
cat("okay\n\n\n")

g =
function(n)
{
   ctr = 0L
   while(TRUE) {
       for(i in 1:n) {
           ctr = ctr + 1L
           printf("%d\n", ctr)
           if(ctr == 35L)
               break
       }
       if(ctr == 35L)
          break
   }
   ctr
}

gc = compileFunction(g, Int32Type, Int32Type)
showModule(gc)
stopifnot(.llvm(gc, 4) == 35)
cat("okay\n\n\n")
