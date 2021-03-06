# Here we are compiling an R function as if it were
# a kernel for a GPU.  
#
#
#

library(RLLVMCompile)

f = function(x)
{
#  idx = threadIdx$x # blockIdx$x * blockDim$x + threadIdx$x
       # the following has issues with off by one. Doesn't use zero counting
       # so ends up writing into element before start of out.
  idx =  blockIdx$x * blockDim$x + threadIdx$x
  x[idx] = idx  #XXX need to avoid the 1 based indexing
#  return()
}

g = function(x)
{
  myblock =   blockIdx$y * gridDim$x  + blockIdx$x
  blocksize = blockDim$x * blockDim$y * blockDim$z
  subthread = threadIdx$z * blockDim$x * blockDim$y + threadIdx$y * blockDim$x + threadIdx$x

  idx = blocksize * myblock + subthread
# x[idx] = 1L
#  idx = 2L
   # indexing x by idx doesn't work, but does if we remove myblock from above
   # Nor does assigning idx as the right hand side.
   # nvvm gives unsupported operation.
#  x[ 1L ] = 1 # idx
  x[ idx ] = idx
}

m = ModuleForNVVM()
f.fun = compileGPUKernel(f, list(x = Int32PtrType), module = m, .zeroBased = c(idx = TRUE))

#Int64PtrType = pointerType(Int64Type)
cat("compiling g\n")
#if(FALSE)
globalFloatPtrType = pointerType(FloatType, , 1L)
g.fun = compileGPUKernel(g, list(x = globalFloatPtrType), module = m, .zeroBased = c(idx = TRUE), 
                         .localVarTypes = list(idx = Int64Type, myblock = Int64Type, blocksize = Int64Type, subthread = Int64Type)
                        )

ptx = tryCatch({
	  library(Rnvvm) 
          code = as(m, "character")
          code = RLLVMCompile:::fixPTXCodeForNVVM(code)
          generatePTX(code)
        }, error = function(e) {
          Rllvm::generatePTX(m)
        })


  library(RCUDA)
  cu.mod = loadModule(I(ptx)) # cuModuleLoadDataEx(ptx)

  N = 32
  ans = integer(N)
  out = .gpu(cu.mod$f, ans, gridDim = 1, blockDim = 32)
  stopifnot(identical(out, 1:N - 1L))

  N = 16^2*2*4
  ans = numeric(N)
  out = .gpu(cu.mod$g, ans, gridDim = c(2, 4), blockDim = c(16, 16))
  stopifnot(identical(out, 1:N - 1))
