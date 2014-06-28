library(RLLVMCompile)

f =
function(l)
{
   lapply(l, g)
}

g = function(x)
{
   x[[1]]
}

h = function(x)
{
   x[[2]]
}

gc = compileFunction(g, SEXPType, list(VECSXPType))
fc = compileFunction(f, VECSXPType, list(VECSXPType), module = as(gc, "Module"))
#                     .routineInfo = list(g = list(SEXPType , VECSXPType)))

if(FALSE) {
.llvm(gc, list(a = TRUE))
.llvm(gc, list(a = 1:10)) 

x = replicate(N, list(a = 1:2, b = "abc"))
.llvm(f, x)
}

    
