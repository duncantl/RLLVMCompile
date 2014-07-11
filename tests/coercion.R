library(RLLVMCompile)

f =
function(i)
{
  i = i + 2
  i
}

fc = compileFunction(f, Int32Type, Int32Type)
stopifnot(.llvm(fc, 3) == 5L)

g =
function(i)
{
  i = 2 + i 
  i
}

gc = compileFunction(g, Int32Type, Int32Type)
stopifnot(.llvm(gc, 3) == 5L)


g =
function(i)
{
  tmp = 2
  i + tmp
}

gc = compileFunction(g, Int32Type, Int32Type)
stopifnot(.llvm(gc, 3) == 5L)
