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
  while(TRUE) {
      ctr = ctr + 1L
      if(ctr == 5L)
          break
  }

  ctr
}


fc = compileFunction(f, Int32Type)
.llvm(fc)
