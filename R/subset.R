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
function(call, env, ir, ..., load = TRUE, SEXPToPrimitive = TRUE)
{
browser()  
  objType = getElementAssignmentContainerType(call, env)
  if(is(objType, "SEXPType")) {  # is this already in compile.=? If so, consolidate.
    if(SEXPToPrimitive) {
      r = getSEXPTypeElementAccessor(objType)
      declareFunction(env$.builtInRoutines[[r]], r, env$.module)
      e1 = substitute(.tmp <- f(x), list(f = as.name(r), x = call[[2]]))
      e2 = substitute(.tmp[i], list(i = call[[3]]))
      compile(e1, env, ir)
      return(compile(e2, env, ir))
    } else
       stop("subsetting SEXPs as SEXPs is not implemented yet")
    }
 
        # ty = getDataType(obj, env)
     # do we need to load this.  The compile.= function 
  obj = getVariable(call[[2]], env, ir, load = FALSE) #???? for load = FALSE

    #XXX Need to handle subsetting generally and need to ensure we get an integer
  call[[3]] = subtractOne(call[[3]])

  i = compile(call[[3]], env, ir) # getVariable(call[[3]], env, ir)
  #i = getVariable(call[[3]], env, ir)
  idx = ir$createSExt(i, 64L)

  ty = getType(obj)
  if(isArrayType(ty)  ||
         (isPointerType(ty) && isArrayType(getElementType(ty))))
    idx = list(createIntegerConstant(0L, getContext(env$.module)), idx)

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
function(call, env, ir, ...)
{
  ll = subsetHandler(call, env, ir, load = FALSE)
#  ir$createStore(tmp, ans.i)
}
  
