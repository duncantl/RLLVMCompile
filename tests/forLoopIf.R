library(RLLVMCompile)

f =
function(n)
{
   ctr = 0L
   for(i in 1L:n) {
       ctr = ctr + 1L
       if(ctr < 100L)
           ctr = ctr + 1L
       else
           ctr = ctr - 1L
   }

   ctr
}

f = function(n) {
  ctr = 0
  for(i in 1:n) {
      ctr = ctr + 1
      if(i == 2L)
         printf("%d %lf\n", i, ctr)
  }

  ctr
}

mod = Module()
fc = compileFunction(f, Int32Type, list(Int32Type), .fixIfAssign = FALSE, module = mod)
showModule(fc)

#.llvm(fc, 3L)
