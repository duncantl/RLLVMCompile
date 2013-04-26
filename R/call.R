callHandler =
  #
  # This handles calls to other functions.
  #
function(call, env, ir, ..., fun = env$.fun, name = getName(fun))
{
 browser()
   funName = as.character(call[[1]])

       #XXX may not want this generally, but via an option in env or just have caller invoke compileSApply() directly.
       #  See fgets.Rdb in Rllvm/
   if(funName == "sapply" &&  isSEXPType(type <- getDataType(call[[2]]))) {
      e = rewriteSApply(call, type, ) # return type of routine being called.
      lapply(e, compile, env, ir, ...)
   }
   
   if(isPrimitiveConstructor(call))  
     return(compilePrimitiveConstructor(funName, call, env, ir, ...))

   # switch and other special functions.
   
   funName = mapRoutineName(funName)

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

