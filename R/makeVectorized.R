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
