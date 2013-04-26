library(RLLVMCompile)
INTSXPType = getSEXPType("INT")
REALSXPType = getSEXPType("REAL")
g =
function(x)
{
   x[2]
}
gc = compileFunction(g, VoidType, list(INTSXPType))
gc = compileFunction(g, Int32Type, list(INTSXPType))
.llvm(gc, 1:3)

gc = compileFunction(g, Int32Type, list(REALSXPType))  # okay
stopifnot(is.integer(.llvm(gc, c(10, 20, 30))))

gc = compileFunction(g, DoubleType, list(REALSXPType)) # okay
stopifnot(.llvm(gc, c(10, 20, 30)) == 20)

  #XX Can't create the cast.
gc = compileFunction(g, DoubleType, list(INTSXPType)) # okay
ans = .llvm(gc, 1:3)
stopifnot(ans == 2 && typeof(ans) == "double") 

###############################

g =
function(x)
{
   x[1] = x[1] + 20L
   x
}
gc = compileFunction(g, INTSXPType, list(INTSXPType))


g =
function(x)
{
  x[1] + 20L
}
gc = compileFunction(g, VoidType, list(INTSXPType))
