getTypes =
function(obj, env, elementType = FALSE)
{
   if(is(obj, "integer"))
      Int32Type
   else if(is(obj, "numeric"))
      DoubleType   
   else if(is.name(obj)) {

     id = as.character(obj)
     ans = if(id %in% names(env$.types))
              env$.types[[as.character(obj)]]
           else
                # look in additional environments if necessary.
              getVariableType(id)
     
     if(elementType)
        getTypeOfElement(ans)
     else
        ans
   } else if(is.call(obj)) {
          # temporarily deal with x[ expr ]
       if(obj[[1]] == as.name("[")) # XXX not in any way general And doesn't handle vectors being returned.
          return(getTypes(obj[[2]], env, TRUE))

       fun = as.character(obj[[1]])
       if(fun == "(")
         return(getTypes(obj[[2]], env))
       
       if(fun %in% names(env$.functionInfo))
         return( get(fun, env$.functionInfo)$returnType )
       else if(fun %in% names(FunctionTypeInfo)) 
         return(getFunctionTypeInfo(fun, obj, env, elementType, FunctionTypeInfo))

       getType(obj, env)

   } else {
     stop("Can't determine type for ", class(obj))
   }
}

getVariableType =
  #
  #  See getVariable() in utils.R and synchronize/merge
  #
function(name, constants = ConstantInfo)
{
#  if(name %in% names(constants))
  v = find(name)
  if(length(v) == 0)
    stop("cannot find variable ", name)

  mapRTypeToLLVM(class(get(name, v[1])))    
}

getFunctionTypeInfo =
function(funName, call, env, elementType = FALSE, info = FunctionTypeInfo)
{
   i = info[[funName]]
   ans = i$"return"
   if(is.character(ans))
      mapRTypeToLLVM(ans)
   else
      ans
}

mapRTypeToLLVM =
function(name)
{
  switch(name,
         "numeric" = DoubleType,
         "integer" = Int32Type,
         stop("don't know mapping from ", name, " to LLVM"))
}


getMathOpType =
# Convenience function for checking types in math ops: if types are
# same, return the common type; it not, return DoubleType (as this
# will be what we should coerce to).
function(types)
{
   if( identical(types[[1]], types[[2]]) )
     return(types[[1]])

   i = match(types, c(Int32Type, DoubleType))
   if(!any(is.na(i)))
      return(DoubleType)
   
}

getTypeOfElement =
  #
  # Given a pointer type or an array type, return the type of the underlying element.
  #
function(type)
{
  if (identical(type, DoublePtrType))
    return(DoubleType)
  else if (identical(type, Int32PtrType))
    return(Int32Type)
  else if (identical(type, FloatPtrType))
    return(FloatType)
  else
    stop("This type is not yet implemented.")
}

# There is an S4 generic getType in llvm. Why not provide methods for that

getType =
function(val, env)
  UseMethod("getType")

getType.character =
function(val, env)
{
  env$.types[[val]]
}

getType.integer =
function(val, env)
{
  Int32Type
}

getType.name =
function(val, env)
  getType(as.character(val), env)

getType.ConstantInt =
function(val, env)  
{
  Int32Type
}

getType.ConstantFP = 
function(val, env)    
{
  DoubleType
}


getType.BinaryOperator =
function(val, env)
{
  Rllvm::getType(val)
}

getType.call = 
function(val, env)
{
  fun = as.character(val[[1]])
  if(fun %in% c('+', '-', '*', '/')) { #XXXX
      types = lapply(val[-1], getTypes, env)
      return(getMathOpType(types))
  }

  warning("can't tell type of call ", paste(deparse(val), collapse = " "))
  NULL
}

getType.default =
function(val, env)
{
#  return(getTypes(val, env))
  if(length(val) == 1)
      mapRTypeToLLVM(class(val))
  else {
    warning("getType for ", class(val), ": default method")
    NULL
  }
}
