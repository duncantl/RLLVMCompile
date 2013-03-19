library(RLLVMCompile)

f = function(x) return(x + 1)

fc = compileFunction(f, DoubleType, DoubleType, asFunction = TRUE)

fc(3)
fc(3L)
fc(TRUE)

# Now, let's vectorize this in x
fc = compileFunction(f, DoubleType, DoubleType, .vectorize = "x", asFunction = TRUE)
#
# Should we use pointerType or arrayType for an R vector.
# The latter requires a length. We can use NA, but have to be careful.
# pointerType doesn't require a length but it is not clear whether we are passing
# a pointer to a single element, or an array. This is the usual C ambiguity.
#  We could introduce a new R type, but it is nice to deal with this in terms of LLVM types.
#  We could also define our own C++ extended/derived type to represent an R vector.
#
# We want to be able to specify the return value in terms of x, the input, e.g.
#  arrayType(getType(x), length(x))
#
#  The TypeInfo package has a construct for this.
#
#
# Should we explicitly add the for loop and then use the existing compiler
# functions to compile that? Or should we add this implicitly? 
#

f = function(x) x + 1

g = function(x){
  ans = numeric(length(x))
  for(i in seq(along = x))
    ans[i] = x[i] + 1
  return(ans)
}

fc = compileFunction(f, arrayType(DoubleType, 0), arrayType(DoubleType, 0), .vectorize = "x", asFunction = TRUE)

#
#
#
#
#
#
