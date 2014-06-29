callRFunction =
function(call, env, ir, ...)
{
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

#   compile(e, env, ir, ...)
  # put the current call into the module with name <funName>.call
  #
  # Potentially, preserve the call by putting it in the environment of the R function that is a proxy for the
  # the LLVM routine.
  id = sprintf("%s_expression", funName)

  llvmAddSymbol(R_GlobalEnv = getNativeSymbolInfo("R_GlobalEnv"))
  createGlobalVariable("R_GlobalEnv", env$.module, SEXPType)

#XXX  We need to specify the caller. If the caller has given us the ExecutionEngine
#  we could set this.  Check !is.null(env$.ExecEngine).
  callVar = createGlobalVariable(id,  env$.module, SEXPType, getNULLPointer(SEXPType))

#  compileSetCall(id, sprintf("setCall_%s", id), env$.module)
 # Add info to .SetCallFuns to have the top-level compiler generate these routines when it is finished. 
env$.SetCallFuns[[ length(env$.SetCallFuns) + 1L]] = list(var = id, name =  sprintf("setCall_%s", id), call = call)

#  if(!is.null(env$.ExecEngine) ) {
#      env$.module[[id, .ee = env$.ExecEngine]] = call
#  }


  e = substitute(r_ans <- Rf_eval(callVar, R_GlobalEnv), list(callVar = as.name(id)))
  compile(e, env, ir, ...)
  # Do we need to protect this?
#  compile(quote(Rf_protect(r_ans)), env, ir, ...)
#compile(quote(Rf_PrintValue(r_ans)), env, ir, ...)
#  compile(quote(Rf_unprotect_ptr(rans)), env, ir, ...)
   # Now get the result and marshall it back
  if(sameType(funTypes[[1]], StringType)) {
        # memory management.
      marshallAns = quote(ans <- strdup(R_CHAR(STRING_ELT(r_ans, 0)))) # XXXX Somebody needs to free this.
  } else  if(sameType(funTypes[[1]], Int32Type)) {
      marshallAns = quote(ans <- Rf_asInteger(r_ans))
  } else  if(sameType(funTypes[[1]], DoubleType)) {
      marshallAns = quote(ans <- Rf_asReal(r_ans))
  } 

  compile(marshallAns, env, ir)
}


# Also in createLoop.R. So put into a function.
# But we don't need them and actually introduced a bug since we spelled r_ans as rans in the Rf_eval() later on
# So better to let the type specification work for us.
createCompilerLocalVariable =
function(name, type, env, ir)
{
  tmp = createFunctionVariable(type, name, env, ir)
  assign(name, tmp, env)
  env$.types[[name]] = type
  tmp
}


compileSetCall =
    #
    # This creates a new function/routine in the module
    #  that allows us to call it from R to set a SEXP type
    # that is a call to a global variable that will then be used
    # to callback to R from an LLVM-generated routine.
function(varName, funName, module)
{
  f = function(tmp) {
    R_PreserveObject(tmp)
    var = tmp
  }
  body(f)[[3]][[2]] = as.name(varName)

# Can we compile a new function while currently in the middle of compiling another with a different IRBuilder.
  compileFunction(f, VoidType, list(SEXPType), module, name = funName)
}


compileCreateCall =
    #
    # Do we want to make this a separate routine
    #
function(env, ir, call, globalVarName = NA, ...)
{
  funName = as.character(call[[1]])

#  createCompilerLocalVariable("zz", SEXPType, env, ir)
#  createCompilerLocalVariable("cur", SEXPType, env, ir)

# if we assign to zz, there is a
#   'module verification: Instruction does not dominate all uses!'
   e = substitute( zz <- Rf_allocVector(LANGSXP, nels) ,  # e <- , LANGSXP     Rf_protect( ) around the entire thing.
                      list(nels = length(call)))
#  browser()
  #XXX This caluses problems
   compile(e, env, ir, ...)
   tmp = substitute(SETCAR(zz, Rf_install(id)), list(id = funName))
   compile(tmp, env, ir, ...)
   compile(quote(cur <- CDR(zz)), env, ir, ...)

   argNames = names(call)
   for(i in seq(along = call)[-1]) {
       tmp = substitute(SETCAR(cur, x), list(x = call[[i]]))
       compile(tmp, env, ir, ...)
       if(length(argNames) && argNames[i - 1L] != "")
          compile(substitute(SET_TAG(cur, Rf_install(id)), id = argNames[i - 1L]), env, ir, ...)

       if(i < length(call))
          compile(quote(cur <- CDR(cur)), env, ir, ...)                                  
   }

   if(!is.na(globalVarName) && globalVarName != "")
     compile(substitute( v <- zz, list(v = as.name(globalVarName))), env, ir, ...)
             
#   compile(quote(Rf_PrintValue(zz)), env, ir, ...)
}

compileCreateCallRoutine =
    #
    # Do we need a new compiler?
    # Do we want to make this a separate routine
    #
function(env, ir, call, name, globalVarName = NA)
{
  f = Function(name, VoidType, list(), module = env$.module)
  b = Block(f, "createCallEntry")
  ir$setInsertBlock(b)
  env$.entryBlock = b  # vital to set this so that the local variables go into this block.
                       # otherwise go into the entry block of the original function being compiled
                       # Need to generalize, e.g. add a method to the compiler to create a new Function
  
  compileCreateCall(env, ir, call, globalVarName)
  ir$createRetVoid()
}
