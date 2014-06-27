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

     #XXX Check if is name and not call/expressions such as foo()
   varName = as.character(call[[2]])

      # if we have a dimensioned object, then, for now, we find how to access
      # the elements (e.g. REAL, INTEGER) in a different way.
   dimensioned = FALSE
   if(length(call) > 2 && varName %in% ls(env$.dimensionedTypes)) {
      dimType = env$.dimensionedTypes[[varName]]
      r = getSEXPTypeElementAccessor(dimType@elType, env)
      dimensioned = TRUE
   } else 
     r = getSEXPTypeElementAccessor(type, env)
     
   fn = env$declFunction(r)

 # For data frames, we have to do things differently

     # So get the variable and then call REAL(), INTEGER(), or whatever on it
     # so that ptr is the collection of elements.
   var = getVariable(varName, env, ir)
   ptr = ir$createCall(fn, var)

     # Now compute the index of the element - not elements.
     # THIS IS NOT VECTORIZED but SCALAR

    call[-(1:2)] = lapply(call[-(1:2)], function(x) if(is.numeric(x) && x == as.integer(x)) as.integer(x) else x)
   

   if(length(call) > 2) {
      i = createMultiDimGEPIndex(call, env, ir, ...)
   } else {
      i = compile(subtractOne(call[[3]]), env, ir)
   }

   idx = ir$createSExt(i, 64L)
   
   gep = ir$createGEP(ptr, idx)
   return(gep)
#
# rest ignored     
##########################################   
         # call INTEGER(call[[1]]), etc.
   e = substitute(.tmp <- r(x), list(r = as.name(r), x = call[[2]]))

   compile(e, env, ir, ...)

    # now have the original call refer to .tmp
   call[[2]] = e[[2]]
   call
}


getSEXPTypeElementAccessor =
function(type, env)
{
   if(is(type, "INTSXPType") || is(type, "LGLSXP")) 
      "INTEGER"
   else if(is(type, "REALSXPType") || sameType(type, DoubleType) ) # This DoubleType is for when we are dealing with a matrix and have the element type.
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



createMultiDimGEPIndex =
function(call, env, ir, ...)
{
      # dealing with a matrix for now. Basically,
      # we have something of the form x[i, j], and i and j could be expressions.
      # We calculate the number of rows in x and then multiply that by j and add i
      # using zero-based calculations for i and j.
      #
      # We also eliminate unnecessary computations if we know at compile time that they are not necesary, e.g.
      # if 1st column, don't need number of rows, if first row, don't need to add row offset.

      ee = substitute( (j - 1L) * Rf_nrows(x) + (i - 1L), list(i = call[[3]], j = call[[4]], x = call[[2]] ))
         # see if we know this is the first column and if so, we don't need the number of rows.
      if(is.numeric(call[[4]]) && call[[4]] == 1L)
          ee = ee[[3]]
      else if(is.numeric(call[[3]]) && call[[3]] == 1L)
           ee = ee[[2]]

      compile(ee, env, ir, ...)
}
