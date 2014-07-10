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

fc = compileFunction(f, Int32Type, optimize = FALSE)
#showModule(fc)
stopifnot(.llvm(fc) == 10L)
cat("okay\n\n\n\n\n")


f =
function()
{
  ctr = 0L
  while(ctr < 10L) {
      ctr = ctr + 1L
      printf("ctr = %d\n", ctr)            
      if(ctr == 5L) 
         break
#      ctr
  }
  
  ctr
}


fc = compileFunction(f, Int32Type, optimize = FALSE)
#showModule(fc)
stopifnot(.llvm(fc) == 5L)
cat("okay\n\n\n\n")



g =
function()
{
  ctr = 0L
  while(ctr < 10L) {
      ctr = ctr + 1L
      printf("ctr = %d\n", ctr)                        
      if(ctr != 5L) 
         next
      printf("BOB\n")
  }
  
  ctr
}


gc = compileFunction(g, Int32Type, optimize = FALSE)
#showModule(fc)
stopifnot(.llvm(gc) == 10L)
cat("okay\n")

