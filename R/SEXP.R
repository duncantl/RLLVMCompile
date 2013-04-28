assignToSEXPElement =
function(call, compiledValue, env, ir, type = getElementAssignmentContainerType(call, env), ...)
{

   if(is(type, "VECSXPType")) {
     stop("not completed yet")
   }

   if(is(type, "STRSXPType")) {

     r = "SET_STRING_ELT"
     findFun(r, env)  # registers it with the module if necessary.

        # we want to modify x[i] = val to SET_STRING_ELT(x, i, val)
        # Do we also need to add the mkChar(). Depends on what the type is of the RHS,
        # i.e. compiledValue. We may need more information here than we have, i.e. args[[2]] from the caller of this function.
#     tmp = compile(compiledValue, env, ir)
#     getType(tmp)
      ty = getDataType(compiledValue, env)
      if(is(ty, "STRSXPType")) {
         findFun("STRING_ELT", env)
         compiledValue = substitute(STRING_ELT(x, 0L), list(x = compiledValue))
      } else if(sameType(ty, StringType)) {
         findFun("Rf_mkChar", env)        
         compiledValue = substitute(Rf_mkChar(x), list(x = compiledValue))
      } else
        stop("what type should this be for putting into a STRSXP?")

     e = substitute(SET_STRING_ELT(x, i, val),
                     list(x = call[[2]], i = subtractOne(call[[3]]),  val = compiledValue))
     compile(e, env, ir)
     return(NULL)
   }

   r = getSEXPTypeElementAccessor(type)

   env$declFunction(r)

         # call INTEGER(call[[1]]), etc.
   e = substitute(.tmp <- r(x), list(r = as.name(r), x = call[[2]]))

   compile(e, env, ir, ...)

    # now have the original call refer to .tmp
   call[[2]] = e[[2]]
   call
}


getSEXPTypeElementAccessor =
function(type)
{
   if(is(type, "INTSXPType") || is(type, "LGLSXP")) 
      "INTEGER"
   else if(is(type, "REALSXPType")) 
      'REAL'
   else
      stop("not done yet")
}


isSEXPType =
function(x)
  is(x, "SEXPType")
