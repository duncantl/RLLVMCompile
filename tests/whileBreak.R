library(RLLVMCompile)

f =
function()
{
  ctr = 0L
  while(ctr < 10L) {
      ctr = ctr + 1L
  }

  ctr
}


f =
function()
{
  ctr = 0L
  while(ctr < 10L) {
      ctr = ctr + 1L
      printf("ctr = %d\n", ctr)            
      if(ctr == 5L) 
          break
  }
  
  ctr
}

#debug(RLLVMCompile:::createConditionCode)

fc = compileFunction(f, Int32Type, optimize = FALSE)
showModule(fc)
.llvm(fc)
