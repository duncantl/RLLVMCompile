callHandler =
  #
  # This handles calls to other functions.
  #
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL)
{
   funName = as.character(call[[1]])

   if(funName == "<-" || funName == "=")
     return(env$.compilerHandlers[["<-"]](call, env, ir, ...))  #XXX should lookup the  or "=" - was `compile.<-`
   else if(funName %in% c("numeric", "integer", "character", "logical")) {
     if(length(call) == 1)
       call[[2]] = 1L #XXX or 0 for an empty vector?
     
     call[[3]] = call[[2]]
     call[[2]] = getSEXPTypeNumByConstructorName(funName)
     call[[1]] = as.name(funName <- "Rf_allocVector")     
   } else if(funName == "$") {
      return(env$.compilerHandlers[["$"]](call, env, ir, ...))
   }


    if(as.character(call[[1]]) %in% names(env$.CallableRFunctions)) {
       return(callRFunction(call, env, ir, ...))
    }

   
       #XXX may not want this generally, but via an option in env or just have caller invoke compileSApply() directly.
       #  See fgets.Rdb in Rllvm/
   if(!is.null(type <- getSApplyType(call, env, funName))) {
      fun = env$.module[[ as.character(call[[3]]) ]]
      rt = getReturnType(fun)
      e = rewriteSApply(call, type, rt, env = env, ir = ir) # return type of routine being called.
      ans = lapply(e, compile, env, ir, ...)
      return(ans[[length(ans)]])
   }

 #XXX Can this happen now that rewrite it above to use Rf_allocaVector()?
   if(isPrimitiveConstructor(call))  
     return(compilePrimitiveConstructor(funName, call, env, ir, ...))

   # switch and other special functions.
   
   funName = mapRoutineName(funName)

    # Here we utilize the polymorphic nature of intrinsics.
    # We may not want this flexibility. e.g. if we have an integer
    # and are calling log(), then we get an int32 returned. We probably
    # want to coerce the input up to a double and use the regular log() fn.
#XXX remove the intrinsics here as problems on Linux.
   if(FALSE && isIntrinsic(funName)) {
      argTypes = lapply(as.list(call[-1]), getTypes, env)
      ofun = getIntrinsic(env$.module, funName, argTypes)
   } else 
      ofun = findFun(funName, env)

   
     #??? Need to get the types of parameters and coerce them to these types.
     # Can we pass this to compile and have that do the coercion as necessary

   targetTypes = getParamTypes(call[[1]], env)
   args = mapply(function(e, ty)
                   compile(e, env, ir, ..., .targetType = ty),  # ... and fun, name,
                 as.list(call[-1]), targetTypes)
   
   env$addCallInfo(funName)
  
   call = ir$createCall(ofun, .args = args)
   if(isTailFunction(env$.Rfun, env$.hints))
     setTailCall(call)

   # If pass an aggregate by value
   #     setArgByVal(call, 1L)
   
   
   call
}

findFun =
function(id, env)
{
  funcs = getModuleFunctions(env$.module)
  if(id %in% names(funcs))
     return(funcs[[id]])
  else if(id %in% names(env$.builtInRoutines)) 
     return(declareFunction(env$.builtInRoutines[[id]], id, env$.module))

  stop("Can't reference function ", id, " in module ") #, getName(env$.module))
}


isTailFunction =
function(fun, hints)
{
  if(inherits(fun, "TailFunction"))
    return(TRUE)

  return(FALSE)
}



compilePrimitiveConstructor =
function(funName, call, env, ir, ...)
{
  if(length(call) > 1)
    warning("ignoring the second argument for call")
  
  val = switch(funName,
               character = "",
               string = "",
               integer = 0L,
               numeric = 0,
               logical = TRUE)
  
   compile(val, env, ir, ...)
}



getParamTypes =
    #
    # name is the name of the routine being called
    #
    # Have to be careful this is not called for an R function.
    # if it is, we have the type information in .CallableRFunctions
    
function(name, env)
{
   f = env$.builtInRoutines[[ as.character(name) ]]
   if(!is.null(f))
      return(f[-1])

   f = env$.module[[ as.character(name) ]]
   lapply(getParameters(f), Rllvm::getType)
}
