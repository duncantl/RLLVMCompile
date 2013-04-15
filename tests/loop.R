#
# Consider this simple function. In R, it would be very slow.
# When we compile it, it should be fast.
# One question is how do we convey the length of the function.
# We can pass x as a double* (pointer to double), and then we have to provide the length
# of the array via a second parameter. We then have to connect this to the for() loop.
# An alternative is to pass x as a SEXP. Then the for() loop code can generate a call
# to Rf_length() to determine the length.
# When we call the compiled routine, we need to be able to pass the SEXP as-is. We could
# use I(), but then we have to add code for this. Instead, we want to be able to declare the 
# type of the parameter as a SEXP and have R understand this. For this, we want to introduce
# a new builtin-type in Rllvm.   We also want to be able to specify the type of SEXP, i.e. numeric
# rather than INTSXP, LGLSXP, VECSXP or CLOSSXP.
# We don't want to extend LLVM to introduce new types. (See http://llvm.org/docs/ExtendingLLVM.html)
# We can define new R variables that have different classes derived from a generic SEXPType.
# The class allows us to determine the correpsonding R type. The instances of the classes
# are unique within a session, but change across sessions.
# We don't actually have to use LLVM pointer types. We could just use our own static types in R
# to identify the target type.
#

if(FALSE) {
  fc = compileFunction(f, DoubleType, list(Rllvm:::getSEXPType("REAL")), name = "f")
}

f <- function(x)
{
   total = 0
   for(val in x)
      total = total + log(x)
   total
}
