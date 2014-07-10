library(RLLVMCompile)

f =
function()
{
  ctr = 0L
  for(i in 1:10) {
      printf("i = %d\n", i)
      if(i == 5L)
          break
  }

  ctr
}

fc = compileFunction(f, Int32Type)
showModule(fc)
.llvm(fc)

g =
function()
{
  ctr = 0L
  for(i in 1:10) {
      if(i == 5L)
          next
      printf("i = %d\n", i)      
  }

  ctr
}

gc = compileFunction(g, Int32Type, .fixIfAssign = FALSE)
cat("\n\n\n\n\nCalling gc\n")
.llvm(gc)
