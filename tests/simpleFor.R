library(RLLVMCompile)

f =
function(n)
{
   ctr = 0L
   for(i in 1:n)
       ctr = ctr + 1L

   ctr
}

fc = compileFunction(f, Int32Type, list(Int32Type))
showModule(fc)

stopifnot(.llvm(fc, 20L) == 20)
