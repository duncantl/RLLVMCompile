getTypes =
function(obj, env, elementType = FALSE, .useFloat = env$.useFloat)
{
   if(is(obj, "integer"))
      Int32Type
   else if(is(obj, "numeric")) {
      if(.useFloat) FloatType else DoubleType   
   } else if(is.character(obj)) {
      StringType
   } else if(is.name(obj)) {

     id = as.character(obj)
     ans = if(id %in% names(env$.dimensionedTypes))
               env$.dimensionedTypes[[id]]
           else if(id %in% names(env$.types))
              env$.types[[ id ]]
           else if(id %in% names(env$.module)) {
              getTypes(getGlobalVariable(env$.module, id), env)
           } else
                # look in additional environments if necessary.
              getVariableType(id)
     
     if(elementType)
        getTypeOfElement(ans)
     else
        ans
   } else if(is.call(obj)) {
          # temporarily deal with x[ expr ]
       if(obj[[1]] == as.name("[")) # XXX not in any way general And doesn't handle vectors being returned.
          return(getTypes(obj[[2]], env, TRUE, .useFloat))

       fun = as.character(obj[[1]])
       if(fun == "(")
         return(getTypes(obj[[2]], env, .useFloat = .useFloat))
       
       if(fun %in% names(env$.functionInfo))
         return( get(fun, env$.functionInfo)$returnType )
       else if(fun %in% names(env$.builtInRoutines)) {
          return(env$.builtInRoutines[[fun]][[1]])
       }
       else if(fun %in% names(FunctionTypeInfo)) 
         return(getFunctionTypeInfo(fun, obj, env, elementType, FunctionTypeInfo, .useFloat = .useFloat))
       else if(fun %in% c("+", "-", "*")) {
          argTypes = lapply(obj[-1], getTypes, env)
          #XXX Make the following more general and more comprehensive test
          if(any(sapply(argTypes, function(x) sameType(x, DoubleType))))
            return(if(.useFloat) FloatType else DoubleType)
          else
            return(argTypes[[1]])
       }

       getDataType(obj, env)

   } else if(is(obj,  "GlobalVariable"))
      getElementType(Rllvm::getType(obj))
   else if(is(obj,  "Value"))
      Rllvm::getType(obj)
   else {
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
function(funName, call, env, elementType = FALSE, info = FunctionTypeInfo, .useFloat = FALSE)
{
   i = info[[funName]]
   ans = i$"return"
   if(is.character(ans))
     mapRTypeToLLVM(ans, .useFloat)
   else
     ans

}

mapRTypeToLLVM =
function(name, .useFloat = FALSE)
{
  switch(name,
         "numeric" = if(.useFloat) FloatType else DoubleType,
         "single" = FloatType,
         "integer" = Int32Type,
         stop("don't know mapping from ", name, " to LLVM"))
}


getMathOpType =
# Convenience function for checking types in math ops: if types are
# same, return the common type; if not, return DoubleType (as this
# will be what we should coerce to).
function(types, values = NULL)
{
   if(length(types) == 1)
     return(types[[1]])

   if(sameType(types[[1]], types[[2]]))
       return(types[[1]])   

   rawTypes = lapply(types, function(x) if(is(x, "Type")) x@ref else x)

#   if( identical(rawtypes[[1]], rawtypes[[2]]) )
#     return(types[[1]])

   ints = c(Int1Type@ref, Int8Type@ref, Int16Type@ref, Int32Type@ref)
   i = match(rawTypes, ints)
   if(!any(is.na(i)))
       return(ints[[ max(i)  ]])

   i = match(rawTypes, c(Int32Type@ref, DoubleType@ref))
   if(!any(is.na(i)))
      return(DoubleType)

   i = match(rawTypes, c(Int8Type@ref, StringType@ref))
   if(all(!is.na(i))) {
       if(length(values)) {
           isChar = sapply(values, function(x) is.character(x) && nchar(x) == 1)
           if(any(isChar))
                return(Int8Type)

           return(StringType)
       }
   }
}

getTypeOfElement =
  #
  # Given a pointer type or an array type, return the type of the underlying element.
  #
function(type)
{

  if(is(type, "RMatrixType"))
       return(type@elType)
    
  if(is(type, "SEXPType")) 
    return(switch(class(type),
                   INTSXPType = Int32Type,
                   LGLSXPType = Int32Type,
                   REALSXPType = DoubleType,
                   STRSXPType = StringType,
                   VECSXP = getSEXPType(),
                   stop("don't know element type of this SEXP")))

  
  if(isPointerType(type))
    return(getElementType(type))
  
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

getDataType =
function(val, env, call = NULL)
  UseMethod("getDataType")

getDataType.AsIs =
function(val, env, call = NULL)
{
   #XXX guessType from Rllvm
  Rllvm:::guessType(val)
}

getDataType.character =
function(val, env, call = NULL)
{
   ty = if(val %in% names(env$.types))
       env$.types[[val]]
   else if(val %in% names(env$.localVarTypes))
       env$.localVarTypes[[val]]
   else {
          # look it up in the module.
       var = env$.module[[ val ]]
       if(is.null(var))
           return(NULL)
       getElementType(getType(var))
   }

   typeFromMetadata(ty, val, env)   
}

typeFromMetadata =
function(ty, id, env)    
{
   if(is(ty, "StringType"))
      return(StringType)
   
#XXXX Use the newer INTSXPType   rather than the metadata, at least first. This will automatically happen
# if the metadata is not found.  But still should just return it unless the metadata is very different.
   if(sameType(ty, SEXPType)) {
       # Try to get more specific SEXP type by looking in the module's metadata.
       md = getMetadata(env$.module, id)
       if(!is.null(md)) {
          ty = as(md[[1]][[1]], "character")
          ty = gsub('[!"]', "", gsub("^metadata ", "", ty))   # the "metadata " is from LLVM 3.5
          return(get(ty, globalenv(), inherits = TRUE))
      }
   }

   ty
   
}

getDataType.integer =
function(val, env, call = NULL)
{
  Int32Type
}

getDataType.name =
function(val, env, call = NULL)
  getDataType(as.character(val), env, call)

getDataType.ConstantInt =
function(val, env, call = NULL)  
{
  Int32Type
}

getDataType.ConstantFP = 
function(val, env, call = NULL)    
{
  DoubleType
}


#getDataType.LoadInst =
getDataType.StoreInst  = getDataType.Value  =
getDataType.BinaryOperator =
function(val, env, call = NULL)
{
  ty = Rllvm::getType(val)
  if(sameType(ty, SEXPType)) {
        # try to make this more specific to the R type.
     if(is.call(call) && as.character(call[[1]]) %in% RewrittenRoutineNames) {

           # These need to be in the function rather than outside as they will be null pointers until Rllvm is loaded.
       ConstructorTypes = list(
         numeric = REALSXPType,
         double = REALSXPType,
         integer = INTSXPType,  
         logical = LGLSXPType,
         character = STRSXPType,
         list = VECSXPType    
         )

       
         ty = ConstructorTypes[[as.character(call[[1]])]]
     }
  }
  ty
}

getDataType.call = 
function(val, env, call = NULL)
{
  fun = as.character(val[[1]])
  if(fun %in% MathOps) {   #XXXX
      types = lapply(val[-1], getTypes, env)
      return(getMathOpType(types))
  }

  mod = env$.module
  if(fun %in% names(mod) && is( f <- mod[[fun]], "Function")) {
    return(getReturnType(f))
  }

  #XXX Builtin types that we know about.
  
  warning("can't tell type of call ", paste(deparse(val), collapse = " "))
  NULL
}

getDataType.default =
function(val, env, call = NULL)
{
#  return(getTypes(val, env))
  if(length(val) == 1)
      mapRTypeToLLVM(class(val))
  else {
    warning("getDataType for ", class(val), ": default method")
    NULL
  }
}


getRVectorFunFromScalar =
function(scalarType)
{
   rtypes = lapply(c(Int32Type, DoubleType, Int8Type), getTypeID)
#   types = list(INTSXPType, REALSXPType, LGLSXPType)
   types = list("integer", "numeric", "logical")
   i = match(getTypeID(scalarType), rtypes)
   if(is.na(i))
       stop("need to match R type for")
   types[[i]]
}

getRVectorTypeFromScalar =
function(scalarType)
{
   rtypes = lapply(c(Int32Type, DoubleType, Int8Type), getTypeID)
   types = list(INTSXPType, REALSXPType, LGLSXPType)
   i = match(getTypeID(scalarType), rtypes)
   if(is.na(i))
       stop("need to match R type for")
   types[[i]]
}


