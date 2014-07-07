library(RLLVMCompile)

f =
function(x)
{
  x * 1
}

fc = compileFunction(f, DoubleType, DoubleType)
showModule(fc)

g =
function(x)
{
  x + 0
}

fc = compileFunction(g, DoubleType, DoubleType)
showModule(fc)


g =
function(x)
{
  x / 1
}

fc = compileFunction(g, DoubleType, DoubleType)
showModule(fc)

g =
function(x)
{
  x - 0
}

fc = compileFunction(g, DoubleType, DoubleType)
showModule(fc)



h =
function(x)
{
  1 + 2 - 3
}

hc = compileFunction(h, DoubleType, DoubleType)
showModule(hc)
