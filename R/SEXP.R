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

   fn = env$declFunction(r)

# Temporary test: Sun, May 5th 7:26
browser()
   var = getVariable(call[[2]], env, ir)
   ptr = ir$createCall(fn, var)
   i = compile(call[[3]], env, ir)
   idx = ir$createSExt(i, 64L)   
   gep = ir$createGEP(ptr, idx)
   return(gep)
#
   
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


# Should  borrow from Rllvm.
STRSXP = 16L
LGLSXP = 9L
REALSXP = 14L
INTSXP = 13L
ANYSXP = 18L
CHARSXP = 9L

getSEXPTypeNumByConstructorName =
function(fun)
{
  switch(fun,
         numeric = REALSXP,
         integer = INTSXP,
         logical = LGLSXP,
         character = STRSXP,
#         list = VECSXP,
         stop("unrecognized function to create an R vector"))
}

getSEXPTypeNum =
function(type)
{
  if(sameType(type, getSEXPType("STR")) || sameType(type, StringType))
     c(STR = STRSXP)
  else if(sameType(type, getSEXPType("REAL")) || sameType(type, DoubleType))
     c(REAL = REALSXP)
  else if(sameType(type, getSEXPType("LGL")))
     c(LGL = LGLSXP)
  else if(sameType(type, getSEXPType("INT")) || sameType(type, Int32Type))
     c(INT = INTSXP)
  else
     stop("don't know what SEXP type corresponds to this type")
}


getSEXPDataAccessor =
function(type)
{
   if(is(type, "SEXPType"))
      return(switch(class(type),
                     REALSXPType = "REAL",
                     INTSXPType = "INTEGER",                    
                     LGLSXPType = "LOGICAL",
                     stop("no accessor for any other type")))
   else
      stop("cannot determine type of SEXP")

 # the following doesn't make sense anymore as we use the same pointer for all the types and only distinguish the SEXPs by the R class.
   if(sameType(type,  getSEXPType("INT")))  
      "INTEGER"
   else if(sameType(type, getSEXPType("LGL"))) 
      "INTEGER"   
   else if(sameType(type, getSEXPType("REAL")))
      "REAL"
   else
     stop("problem getting R data accessor routine name")
}
