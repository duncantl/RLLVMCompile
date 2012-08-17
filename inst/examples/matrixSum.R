# note that x is never changed

matSum = 
function(x)
{
  total = 0
  for(i in 1:nrow(x))
    for(j in 1:ncol(x)) {
        total = total + x[i,j]
    }
  return(total)
}

if(FALSE) {
  compileFunction(matSum, DoubleType, list(x = matrix[DoubleType]))
  compileFunction(matSum, DoubleType, list(x = numericMatrix))

  # identify that nrow() and ncol() of x are invariants
  # within the entire function and so compute them once
  # this can be done as default values for new parameters
  #  function(x, nrow = nrow(x), ncol = ncol(x))
  # and have these be computed in the R function that calls
  # the LLVM function.
                  
  # We have to compile x[i,j] into x[ i * j * nrow ]  and we need
  # to have nrow computed for this.
  # When we detect that we need this, we can create a variable, say x_nrow
  # (assuming there is no conflict).  We can put this at the beginning of the
  # the function or of the block that uses it. For this we have to be able
  # to a) get a reference to the block, b) insert into an existing block before
  # the terminator of that block.                 
                  
#  compileFunction(matSum, DoubleType, list(x = DoubleType))
}
