
subsetDoubleHandler =
function(call, env, ir, ..., load = TRUE, SEXPToPrimitive = TRUE, .targetType = NULL)
{
  if(is.numeric(call[[3]]))
      call[[3]] = as.integer(call[[3]])
  e = substitute(VECTOR_ELT(x, i), list(x = call[[2]], i = subtractOne(call[[3]])))
  compile(e, env, ir, ...)
#  objType = getElementAssignmentContainerType(call, env)
#  index = compile(call[[3]], env, ir, ...)
  
}
    
subsetHandler =
#
# Attempt to handle subsetting a vector of any type.
#
#  Need to make this understand SEXP types and need to differentiate between expecting it back as a native element or a SEXP with one element.
#
#  Also handle vector subsetting.  
#
# References:
#  - GEP: http://llvm.org/docs/LangRef.html#i_getelementptr
#  - SExt: http://llvm.org/docs/LangRef.html#i_sext
function(call, env, ir, ..., objType = getElementAssignmentContainerType(call, env), load = TRUE, SEXPToPrimitive = TRUE, .targetType = NULL)
{
  if(length(call) > 3)
                    # perhaps make this a separate method and have the generic dispatcher call it.
      return(multiSubset(call, env, ir, ..., load = load, SEXPToPrimitive = SEXPToPrimitive))

  if(is(objType, "SEXPType")) {  # is this already in compile.=? If so, consolidate.
    if(SEXPToPrimitive) {
      r = getSEXPTypeElementAccessor(objType)
      declareFunction(env$.builtInRoutines[[r]], r, env$.module)

      e1 = substitute(.tmp <- f(x), list(f = as.name(r), x = call[[2]]))
      e2 = substitute(.tmp[i], list(i = call[[3]]))

      if(length(env$.loopStack)) {
          # lift this accessor to the entry block and create as a non-loop local variable
          # If this is in a nested loop, we have to be careful in case the variable is loop-local
          # i.e. in the parent loop.
          # If we are subsetting a parameter, no problem, as long as it is a simple x[].
          # If it is x[i][j] then we have to be more careful.
          #

          cur = getInsertBlock(ir)
                # Need to put new code before the existing terminator in the entry block.
          term = getTerminator(env$.entryBlock)
          eraseFromParent(term, FALSE)
          setInsertBlock(ir, env$.entryBlock)
          tmpVarName = e2[[2]] = e1[[2]] =  as.name(sprintf("%s.%s", r, as.character(call[[2]]))) # make a fake name
          compile(e1, env, ir)
          insertAtEnd(term, env$.entryBlock)
          setInsertBlock(ir, cur)
      } else 
          compile(e1, env, ir)
      
      return(compile(e2, env, ir))

    } else
       stop("subsetting SEXPs as SEXPs is not implemented yet")
    }
 
        # ty = getDataType(obj, env)
     # do we need to load this.  The compile.= function 
  obj = getVariable(call[[2]], env, ir, load = TRUE) #???? for load = FALSE. Now back to TRUE. Based on fgets.Rdb.

    #XXX Need to handle subsetting generally and need to ensure we get an integer

  zeroBased = is.name(call[[3]]) && as.character(call[[3]]) %in% names(env$.zeroBased)

  if(!zeroBased)
     call[[3]] = subtractOne(call[[3]])

  i = compile(call[[3]], env, ir, isSubsetIndex = TRUE) # getVariable(call[[3]], env, ir)
  #i = getVariable(call[[3]], env, ir)
  idx = ir$createSExt(i, 64L)

  ty = getType(obj)
  if(isArrayType(ty)  || 
         (isPointerType(ty) && isArrayType(getElementType(ty))))
    idx = list(createIntegerConstant(0L, getContext(env$.module)), idx)

  if(!isPointerType(ty) && !isArrayType(ty))
      stop("attempting to create a GEP for a non-pointer type")

  p = ir$createGEP(obj, idx)

  if(load)
    return(ir$createLoad(p))
  
  return(p)
}


subtractOne =
function(k)
{  
 if(is(k, "numeric"))
    return(as.integer(k - 1L))

  tmp = quote(a - 1L)
  tmp[[2]] = k
  tmp
}


subsetAssignHandler =
  #
  # Never used!  Use compile.<- in compile.R
  # It may be good to split that part out to element assignment but okay for now.
  #
function(call, env, ir, ..., .targetType = NULL)
{
  ll = subsetHandler(call, env, ir, load = FALSE)
#  ir$createStore(tmp, ans.i)
}
  
