library(RLLVMCompile)

f =
function(n)
{
   ctr = 1L
   if(n < 10L)
       ctr = ctr + n
   else
       ctr = ctr - n

   if(ctr > 3L)
       ctr = ctr + 10L

   ctr
}

fc = compileFunction(f, Int32Type, list(Int32Type), .fixIfAssign = FALSE)
showModule(fc)

stopifnot(.llvm(fc, 10) == -9)  # -9
stopifnot(.llvm(fc, 9) == 20)   # 20
