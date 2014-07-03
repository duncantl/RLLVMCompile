library(RLLVMCompile)

f =
function(x)
{
  if(FALSE) {
      foo(x)
  }
  x + 1L
}


g =
function(x)
{
  if(FALSE) {
      foo(x)
  } else {
      ctr = 10L
      x + ctr
  }
}


fc = compileFunction(f, VoidType, list(Int32Type))

fc1 = compileFunction(f, Int32Type, list(Int32Type), name = "f2")

gc = compileFunction(g, Int32Type, list(Int32Type))
