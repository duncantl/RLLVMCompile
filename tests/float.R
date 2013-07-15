library(RLLVMCompile)

Dnorm = function(x, mu = 0, sd = 1, out) {
        myblock = blockIdx$x + blockIdx$y * gridDim$x
        blockSize = blockDim$x * blockDim$y * blockDim$z
        subthread = threadIdDx$z * (blockDim$x * blockDim$y) + threadIdx$y*blockDim$x + threadIdx$x

        idx = myblock * blocksize + subthread
        
        out[idx] = 1/sqrt(2 * pi * sd^2) * exp( - .5*( (x-mu)/sd ) ^2)
       }


Dnorm = function(x, mu = 0, sd = 1) 
         1/sqrt(2 * pi * sd^2) * exp( - .5*( (x-mu)/sd ) ^2)


Dnorm = function(x, mu = 0, sd = 1) 1/2 * pi * sd^2 * exp( - .5*( (x-mu)/sd ) ^2)

# Shows that we collapse constant expressions
#Dnorm = function(x, mu = 0, sd = 1) 3 * 4 

fc = compileFunction(Dnorm, FloatType, list(FloatType, FloatType, FloatType),
                      .builtInRoutines = RLLVMCompile:::getBuiltInRoutines(useFloat = TRUE), .useFloat = TRUE)


# setMetadata(as(fc, "Module"), "nvvm.annotation", list(fc, "kernel", 1L))
