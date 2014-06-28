callRFunction =
function(call, env, ir, ...)
{
browser()
 
   funName = as.character(call[[1]])

   funTypes = env$.CallableRFunctions[[funName]]

   # We have the call to the R function. But we need to match the arguments.
   # We have to convert regular objects to R objects. However, when we have a SEXPType
   # that hasn't been updated within the routine, we can pass this directly.

# We can generate code to create the call. Alternatively, within the same R session
# we can R_PreserveObject the call and then reuse it at call-time in the compiled code.
# We have to fill in the arguments for each call.
# We want to create the call expression just once, regardless if we pass it from the compiler
# or create it ourselves.
# How do we assign an R object to a global variable in a module.
# We could serialize the call to a byte stream and restore it in the module.

   # This is a very 

# Use getVariable.
# Need to compile any arguments that are expressions.

#  sym = substitute(Rf_install(id), list(id = funName))
#  e = substitute(Rf_protect( e <- allocVector(LANGSXP, nels) ),
#                   list(nels = nargs + 1L))
#  quote(cur <- e)
#  quote(SET_CAR(e, sym))
#  quote( cur <- CDR(e))
#
#  for(i in seq(along = call)[-1]) {
#
#  }

#   compile(e, env, ir, ...)
  # put the current call into the module with name <funName>.call
  #
  # Potentially, preserve the call by putting it in the environment of the R function that is a proxy for the
  # the LLVM routine.
  id = sprintf("%s.expression", funName)
#XXX fix
  callVar = createGlobalVariable(id,  env$.module, SEXPType, call)
  e = quote(rans <- Rf_eval(callVar, R_GlobalEnv))
  compile(e, env, ir, ...)

   # Now get the result and marshall it back
  if(sameType(funTypes[[1]], StringType)) {
        # memory management.
      marshallAns = quote(ans <- strdup(R_CHAR(STRING_ELT(r_ans, 0))))
  } else  if(sameType(funTypes[[1]], Int32Type)) {
      marshallAns = quote(ans <- INTEGER(r_ans)[1])
  }

  compile(marshallAns, env, ir)
}
