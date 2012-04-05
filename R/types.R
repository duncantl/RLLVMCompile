getTypes =
function(obj, env, elementType = FALSE)
{
   if(is(obj, "integer"))
      Int32Type
   else if(is(obj, "numeric"))
      DoubleType   
   else if(is.name(obj)) { 
     ans = env$.types[[as.character(obj)]]
     if(elementType)
        getTypeOfElement(ans)
     else
        ans
   } else if(is.call(obj)) {
          # temporarily deal with x[ expr ]
       if(obj[[1]] == as.name("[")) # XXX not in any way general And doesn't handle vectors being returned.
          return(getTypes(obj[[2]], env, TRUE))

   } else {
     stop("Can't determine type for ", class(obj))
   }
     
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

# There is an S4 generic getType in llvm. Why not proide methods for that

getType =
function(val, env)
  UseMethod("getType")

getType.character =
function(val, env)
{
  env$.types[[val]]
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

getType.default =
function(val, env)
  return(NULL)
