getTypes =
function(obj, env, elementType = FALSE)
{
   if(is(obj, "integer"))
      Int32Type
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
   if(identical(type, DoublePtrType))
      return(DoubleType)
   else
      stop("Need more implementation of the type of a pointer type")
}
