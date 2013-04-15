# Consider this simple function below. In R, it would be very slow.
# When we compile it, it should be fast.
#
# When compiling the function, we need to access the individual elements of the
# vector x (which are of type double in LLVM) and we also need to get the length of the
# R vector.  So we need the SEXP from which we can compute the length, and we need access
# to the array of double elements, i.e. stripping away the SEXP information and accessing
# the actual data in the SEXP.
#
# So one question is how do we convey the length of the function?
# We have different approaches to this.
# We can pass x as a double* (pointer to double), and then we have to provide the length
# of the array via a second parameter. We then have to connect this to the for() loop.
# An alternative is to pass x as a SEXP. Then the for() loop code can generate a call
# to Rf_length() to determine the length.
#
# !Note: Unfortunately, Rf_length is not available on Linux via getNativeSymbolInfo().
#
# It is slightly easier in some ways to pass the double * and length as 2 arguments.
# Unfortunately, it is not entirely general. If x is computed somehow in the function
# as a SEXP and then we need its length, we clearly cannot pass the length to the
# function when we call it.
#
# So it is reasonable to try to rewrite the code as f1.
# In C, we would write the loop as
#    for(i = 1, val = x[i]; i < len; i++, val = x[i]) { ... }
#
#
#
#
# * Passing SEXPs to LLVM
# When we call the compiled routine, we need to be able to pass the SEXP as-is. We could
# use I(), but then we have to add code for this. Instead, we want to be able to declare the 
# type of the parameter as a SEXP and have R understand this. For this, we want to introduce
# a new builtin-type in Rllvm.   We also want to be able to specify the type of SEXP, i.e. numeric
# rather than INTSXP, LGLSXP, VECSXP or CLOSSXP.
# We don't want to extend LLVM to introduce new types. (See http://llvm.org/docs/ExtendingLLVM.html)
# We can define new R variables that have different classes derived from a generic SEXPType.
# The class allows us to determine the correpsonding R type, not just a generic SEXP. The instances of the classes
# are unique within a session, but change across sessions.
# We don't actually have to use LLVM pointer types. We could just use our own static types in R
# to identify the target type. However, it is good to try to indicate the actual structure of the
# data types on which we are operating. So we define a simple struct for a SEXP structure, and then
# define a SEXP  type as a pointer to it, and then new types for logical, integer, numeric, character/string, list, etc.
#

library(RLLVMCompile)
f <- function(x)
{
   total = 0
   for(val in x)
      total = total + log(val)
   total
}


f1 <- function(x)
{
   len = length(x)
   x_ = REAL(x)
   total = 0
   for(i in 1:len) {
      val = x_[i]
      total = total + log(val)
   }
   total
}

f2 <- function(x, len = length(x))
{
   total = 0
   for(i in 1:len) {
      val = x[i]
      total = total + log(val)
   }
   total
}

doublePointer = pointerType(DoubleType) 
if(FALSE) {
  fc = compileFunction(f2, DoubleType, list(doublePointer, Int32Type), name = "f")
}




if(FALSE) {
  fc = compileFunction(f, DoubleType, list(Rllvm:::getSEXPType("REAL")), name = "f")
}


if(FALSE) {
  x = c(1, 2, 3, 4)
  x = rnorm(1000, 20)
  a = sum(log(x))
  b = .llvm(fc, x, length(x))
  identical(a, b) # false - difference in 11th - 16th place.  Is this the fault of the intrinsic??
}
