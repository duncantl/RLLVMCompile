library(RLLVMCompile)
#INTSXPType = getSEXPType("INT")
#REALSXPType = getSEXPType("REAL")

g =
function(x)
{
   x[2]
}
#gc = compileFunction(g, VoidType, list(INTSXPType))
gc = compileFunction(g, Int32Type, list(INTSXPType))
.llvm(gc, 1:3)

gc = compileFunction(g, Int32Type, list(REALSXPType))  # okay
stopifnot(is.integer(.llvm(gc, c(10, 20, 30))))

gc = compileFunction(g, DoubleType, list(REALSXPType)) # okay
stopifnot(.llvm(gc, c(10, 20, 30)) == 20)

  #
gc = compileFunction(g, DoubleType, list(INTSXPType)) # okay
ans = .llvm(gc, 1:3)
stopifnot(ans == 2 && typeof(ans) == "double") 

###############################


g =
function(x)
{
  x[1] + 20L
}
gc = compileFunction(g, VoidType, list(INTSXPType))
stopifnot(is.null(.llvm(gc, 1:3)))



# This seems to be getting the code wrong, let alone not being able to return. But in fact it is
# exploiting the fact that this is the first element and so doesn't need the getelementptr.
g =
function(x)
{
   x[1] = x[1] + 20L
   x
}
gc = compileFunction(g, INTSXPType, list(INTSXPType))
stopifnot(identical(.llvm(gc, 1:3), c(21L, 2L, 3L)))


# We can see the getelementptr
g =
function(x)
{
   x[1] = x[2] + 20L
   x
}
gc = compileFunction(g, INTSXPType, list(INTSXPType))
stopifnot(identical(.llvm(gc, 1:3), c(22L, 2L, 3L)))



