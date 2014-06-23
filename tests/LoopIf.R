library(RLLVMCompile)

f =
function(x, n)
{
  ctr = 0L
  for(i in 1:n) {
      if(x[i] > 2)
        ctr = ctr + 1L
  }
  ctr
}
