vectorizeFunction =
#
#  just implement this as 
#    function(.........)
#      sapply(x, fun, ...)
#
#  Would like to not have to specify the types.
#  If given the compiled version of f, then we can get the types.
#
#  vectorizeFunction(Dnorm, scalar = "Dnorm")
#
function(f, fc = NULL, typeInfo = NULL, module = as(fc, "Module"), 
         scalarFunName = getName(fc), vectorArgName = names(parms)[1])
{
  g = f
  parms = formals(f)
  e = substitute(sapply(x, f), list(f = as.name(scalarFunName), x = as.name(vectorArgName)))
  if( (nargs <- length(parms)) > 1) 
      e[seq(4, length = nargs - 1L) ] = lapply(names(parms)[seq(2, length = nargs - 1L)], as.name)

  body(g) = e
  if(length(typeInfo)) {

  } else
     g	 
}


vectorizeScalarFunction =
function(f, fc = NULL, typeInfo = NULL, module = as(fc, "Module"), 
         scalarFunName = getName(fc), vectorArgName = names(parms)[1],
         name = sprintf("%s_v", scalarFunName), ...)
{
   params = getParameters(fc)

      # Get the correspoding R vector type for the scalar type.
   tyIn = getType(getParameters(fc)[[1]])
   tyOut = getReturnType(fc)
   rtype = getRVectorFunFromScalar(tyOut)
       
   lfun = loopifyScalarFun(f, scalarFunName, rtype)
   types = c(x = REALSXPType, lapply(params[-1], getType)) #XXX get the correct type
   tyOut = getRVectorTypeFromScalar(tyOut)
   tyOut = REALSXPType
browser()   
   
   compileFunction(lfun, tyOut, types,  module = module, name = name, ...)    
}

loopifyScalarFun =
    # Given a scalar function, create an R function that vectorizes it via a loop.
    # That is so that it can be compiled relatively easily to yield a vectorized
    # version of the scalar routine.
function(f, scalarFunName, rvecType = "numeric")
{    
   g = function(x) {
       n = length(x)
       ans = vec(n)
       for(i in 1:n) 
           ans[i] = foo(x[i])

       ans
   }

   b = body(g)
   b[[3]][[3]][[1]] = as.name(rvecType)

   rparams = formals(f)
   b[[4]][[4]][[3]][[1]] = as.name(scalarFunName)
   if(length(rparams) > 1)
           # may need to completely change the call if vectorArgName does not correspond to first argument.
       b[[4]][[4]][[3]][seq(3, length = length(rparams) -1L)] =  lapply(names(rparams)[-1], as.name) 

#   b[[4]][[4]] = quote(printf("%d\n", i))
   body(g) = b
   formals(g) = formals(f)
   g
}
