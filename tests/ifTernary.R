library(RLLVMCompile)

f =
function(n)
{
  x = if(n > 10) 3L else 100L
  x
}

fixIfAssign(f)
fixIfAssign(f, addPragma = TRUE)
