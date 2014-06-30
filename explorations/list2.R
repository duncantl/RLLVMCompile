library(RLLVMCompile)

f =
function(l)
{
   lapply(l, g)
}

g = function(x)
{
#   Rf_PrintValue(x)
   x[[1]]
}

h = function(x)
{
   x[[2]]
}

f.h =
function(l)
{
   lapply(l, h)
}

gc = compileFunction(g, SEXPType, list(VECSXPType))
fgc = compileFunction(f, VECSXPType, list(VECSXPType), module = as(gc, "Module"))
#                     .routineInfo = list(g = list(SEXPType , VECSXPType)))

hc = compileFunction(h, STRSXPType, list(VECSXPType), module = as(gc, "Module"))
# Why not a STRSXP rather than VECSXP.  Because we are getting the type from h as a Function
# and that returns the generic SEXPType, not STRSXPType. So we want to collect this information
# in the same compiler. 
fhc = compileFunction(f.h, STRSXPType, list(VECSXPType), module = as(gc, "Module"))


if(FALSE) {
.llvm(gc, list(a = TRUE))
.llvm(gc, list(a = 1:10)) 

N = 100
x = replicate(N, list(a = 1:2, b = "abc"), simplify = FALSE)
z = .llvm(fgc, x)
length(z)
all(sapply(z, length) == 2)


z = .llvm(fhc, x)
}

    
