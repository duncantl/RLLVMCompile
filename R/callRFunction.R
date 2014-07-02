callRFunction =
function(call, env, ir, ...)
{
   funName = as.character(call[[1]])

   funTypes = env$.CallableRFunctions[[funName]]

   if(funName == ".R") {
       if(length(call) > 1)
          funTypes = eval(call[[3]])
       call = call[[2]]
    }

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
env$.SetCallFuns[[ length(env$.SetCallFuns) + 1L]] = createCall = list(var = id,
                                                                       name =  sprintf("setCall_%s", id),
                                                                       createCallFun = sprintf("create_%s", id),
                                                                       deserializeCallFun = sprintf("deserialize_%s", id),   
                                                                       call = call)


   cc = Function(createCall$createCallFun, SEXPType, list(), module = env$.module)
   cc = Function(createCall$deserializeCallFun, SEXPType, list(), module = env$.module)   
   e = substitute( if( var == NULL ) var <- mk(),
                   list(var = as.name(createCall$var), mk = as.name(createCall$deserializeCallFun),
                        msg = sprintf("calling %s\n", createCall$createCallFun)))
   env$.remainingExpressions = list(NULL) #XXXX       
   compile(e, env, ir, ...)

#  if(!is.null(env$.ExecEngine) ) {
#      env$.module[[id, .ee = env$.ExecEngine]] = call
#  }


   # Now, generate the code that puts the local variables into the call.
  insertLocalValues(call, id, env, ir, ...)

#!!! debug: show the updated expressions.
#compile(substitute(Rf_PrintValue(var), list(var = as.name(id))) , env, ir, ...)
   
   # Then evaluate the call
  e = substitute(r_ans <- Rf_eval(callVar, R_GlobalEnv), list(callVar = as.name(id)))
  val = compile(e, env, ir, ...)
   
# Do we need to protect this return value?
#  compile(quote(Rf_protect(r_ans)), env, ir, ...)
#  compile(quote(Rf_unprotect_ptr(rans)), env, ir, ...)
   
   # Now get the result and marshall it back
  if(sameType(funTypes[[1]], StringType)) {
        # memory management.
      marshallAns = quote(ans <- strdup(R_CHAR(STRING_ELT(r_ans, 0)))) # XXXX Somebody needs to free this.
  } else  if(sameType(funTypes[[1]], Int32Type)) {
      marshallAns = quote(ans <- Rf_asInteger(r_ans))
  } else  if(sameType(funTypes[[1]], DoubleType)) {
      marshallAns = quote(ans <- Rf_asReal(r_ans))
  } else if(sameType(funTypes[[1]], SEXPType)) {
    return(val)   
  } else {
      stop("don't know how to handle this type")
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

    # Rf_protect( ) around the entire thing ?
   e = substitute( zz <- Rf_allocVector(LANGSXP, nels) ,  # LANGSXP   
                      list(nels = length(call)))
   compile(e, env, ir, ...)

   tmp = substitute(SETCAR(zz, Rf_install(id)), list(id = funName))
   compile(tmp, env, ir, ...)
   compile(quote(cur <- CDR(zz)), env, ir, ...)

   argNames = names(call)

   for(i in seq(along = call)[-1]) {
       val = if(is.name(call[[i]]))
                 substitute(Rf_install(x), list(x = as.character(call[[i]])))
             else if(is.integer(call[[i]]))
                 substitute(Rf_ScalarInteger(x), list(x = call[[i]]))
             else if(is.numeric(call[[i]]))
                 substitute(Rf_ScalarReal(x), list(x = call[[i]]))
             else if(is.logical(call[[i]]))
                 substitute(Rf_ScalarLogical(x), list(x = call[[i]]))
             else if(is.character(call[[i]]))
                 substitute(Rf_mkString(x), list(x = call[[i]]))
             else {
                  warning("cannot recreate non-local value in R expression")
                  quote(Rf_install("<expr>"))
             }
           
       tmp = substitute(SETCAR(cur, val), list(val = val))
       compile(tmp, env, ir, ...)
       if(length(argNames) && argNames[i] != "") {
          compile(substitute(SET_TAG(cur, Rf_install(id)), list(id = argNames[i])), env, ir, ...)
       }

       if(i < length(call))
          compile(quote(cur <- CDR(cur)), env, ir, ...)                                  
   }

   if(!is.na(globalVarName) && globalVarName != "") {
       e = substitute( if( var != NULL ) R_ReleaseObject(var), list(var = as.name(globalVarName)))
     set = substitute( v <- zz, list(v = as.name(globalVarName)))
     env$.remainingExpressions = list(set) #XXX horrible. Needed to get the NextBlock
     compile(e, env, ir, ...)
     compile(set, env, ir, ...)
   }

   compile(quote(return(zz)), env, ir, ...) # ir$createRet(ir$createLoad(getVariable()))
  
 #  compile(quote({printf("expression: "); Rf_PrintValue(zz)}), env, ir, ...)
}

compileCreateCallRoutine =
    #
    # Do we need a new compiler?
    # Do we want to make this a separate routine
    #
function(env, ir, call, name, globalVarName = NA)
{
    # The function should have already been declared in callRFunction.
    # But here it is as it used to be:
    #        f = Function(name, VoidType, list(), module = env$.module)
  compilerStartFunction(env, ir, name, SEXPType)

  compileCreateCall(env, ir, call, globalVarName)
}

createDeserializeCall =
function(env, ir, call, name, globalVarName = NA, ...)
{
  compilerStartFunction(env, ir, name, SEXPType)    

  llvmAddSymbol(getNativeSymbolInfo("R_loadRObjectFromString", "RLLVMCompile"))

   # serialize the call to a string
  txt = saveRObjectAsString(call)
     # create call to the C routine to deserialize this and compile this expression
  e = substitute(R_loadRObjectFromString(x), list(x = txt))
  v = compile(e, env, ir, ...)
  ir$createRet(v)
}


insertLocalValues =
    #
    # Given the call template, generate the code to insert local variables into 
    # the 
function(call, callVar, env, ir, ...)
{

    # For now, only deal with literals, symbols and calls!
    # Probably just loop over all of them and determine if they involve local variables.
  call = call[-1]
  w = sapply(call, function(x) is.name(x) || is.call(x))

  if(any(w)) {
        # need to CDR() up to the first element. Then insert the value and go to the next one.
        # Get the first argument, i.e. skip over the function name/obj.
      compile(substitute(el <- CDR(v), list(v = as.name(callVar))), env, ir, ...)
      jmp = substitute(el <- CDR(el), list(call = as.name(callVar)))
      e = quote(SETCAR(el, val))
      
      for(i in seq(along = w)) {
          if(w[i]) {
              varName = call[[i]]

          usesLocal = usesLocalVariables(call[[i]], env)
          if(usesLocal) {
              if(is.call(varName)) {
                  var = compile(substitute(.tmp <- e, list(e = call[[i]])), env, ir, ...)
                  varName = as.name(".tmp")
              } else {
                 var = getVariable(as.character(varName), env, load = FALSE, searchR = FALSE)
                 if(!is(var, "Argument"))
                    var = ir$createLoad(var)
              }
              
              ty = Rllvm::getType(var)
              v = if(sameType(ty, Int32Type))
                  substitute(Rf_ScalarInteger(x), list(x = varName))
              else if(sameType(ty, DoubleType))
                  substitute(Rf_ScalarReal(x), list(x = varName))
              else if(sameType(ty, StringType))
                  substitute(Rf_mkString(x), list(x = varName))              
              else if(sameType(ty, SEXPType))
                  as.name(varName)
              else
                  stop("don't know how to handle this type yet")
              
              e[[3]] = v
              compile(e, env, ir, ...)
          } # end of usesLocal
              compile(jmp, env, ir, ...)
          }
      }
  }
}

usesLocalVariables =
function(expr, env)
{
 f = function() e
 body(f) = expr
 info = findGlobals(f, FALSE)
 localVarNames = c(names(env$.localVarTypes),  env$.params@names, names(env$.types))
 any(info$variables %in%  localVarNames)
}





# NOT USED
isRAtomic =
function(x)
   is.logical(x) || is.integer(x) || is.numeric(x) || is.character(x)

