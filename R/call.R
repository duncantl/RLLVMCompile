callHandler =
  #
  # This handles calls to other functions.
  #
function(call, env, ir, ..., fun = env$.fun, name = getName(fun))
{
   funName = mapRoutineName(as.character(call[[1]]))

    # Here we utilize the polymorphic nature of intrinsics.
    # We may not want this flexibility. e.g. if we have an integer
    # and are calling log(), then we get an int32 returned. We probably
    # want to coerce the input up to a double and use the regular log() fn.
   if(isIntrinsic(funName)) {
      argTypes = lapply(as.list(call[-1]), getTypes, env)
      ofun = getIntrinsic(env$.module, funName, argTypes)
   } else 
      ofun = findFun(funName, env)
     #??? Need to get the types of parameters and coerce them to these types.
     # Can we pass this to compile and have that do the coercion as necessary
   args = lapply(as.list(call[-1]), compile, env, ir, ...)  # ... and fun, name,

   call = ir$createCall(ofun, .args = args)
   if(isTailFunction(env$.Rfun, env$.hints))
     setTailCall(call)
   call
}

findFun =
function(id, env)
{
  funcs = getModuleFunctions(env$.module)
  if(id %in% names(funcs))
     funcs[[id]]
  else
    stop("Can't reference function ", id, " in module ") #, getName(env$.module))
}


isTailFunction =
function(fun, hints)
{
  if(inherits(fun, "TailFunction"))
    return(TRUE)

  return(FALSE)
}
