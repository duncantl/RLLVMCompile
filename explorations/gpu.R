# Here we are compiling an R function as if it were
# a kernel for a GPU.  
#
#
#

library(RLLVMCompile)

f = function(x)
{
  idx = threadIdx$x # blockIdx$x * blockDim$x + threadIdx$x
  x[idx] = idx  #XXX need to avoid the 1 based indexing
#  return()
}

fun = compileGPUKernel(f, list(x = Int32PtrType))


if(require(Rnvvm)) {
m = as(fun, "Module")

code = as(m, "character")
code = RLLVMCompile:::fixPTXCodeForNVVM(code)
ptx = generatePTX(code)

library(RCUDA)
cu.mod = cuModuleLoadDataEx(ptx)

N = 32
ans = integer(N)
out = .gpu(cu.mod$f, ans, gridDim = 1, blockDim = 32)
}