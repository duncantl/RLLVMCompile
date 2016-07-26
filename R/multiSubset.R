multiSubset =
    # lots of overlapping/common code with subsetHandler. Consolidate.
function(call, env, ir, ..., load = TRUE, SEXPToPrimitive = TRUE)
{
   varName = as.character(call[[2]])
   dimType = NULL
   if(varName %in% names(env$.dimensionedTypes)) {
        dimType = env$.dimensionedTypes[[varName]]
#        r = getSEXPTypeElementAccessor(dimType@elType, env)
        dimensioned = TRUE        
   } else
        stop("need type and dimension information for ", varName)


   if(is(dimType, "DataFrameType")) {
#      ee = substitute(x[[i]][j], list(x = call[[2]], i = call[[3]], j = call[[4]]))
          #XXX should really call the [[ method in env handlers.
       tmp = substitute( z[[i]], list(z = call[[2]], i = call[[4]]))
       var =  subsetDoubleHandler(tmp, env, ir, ...)
         # then get the i-th element. We have the specific type of this element in the data frame.
         # So we can use this to perform the subsetting.
         #
       ty = dimType@elTypes[[ call[[4]] ]]
       vv = compile(substitute(var[j], list(var = var, j = call[[3]])), env, ir, ..., objType = ty)
       return(vv)
   } else if(is(dimType, "RMatrixType")) {

       if(sameType(dimType@elType, StringType)) {

             # compute the offset expression and the
             # STRING_ELT(x, i)
           idx = compileMatrixOffset(call, env, ir, ..., asSEXT = FALSE)
           e = substitute(STRING_ELT(v, i), list( i = idx, v = call[[2]]))
           return(compile(e, env, ir, ...))
       }

# See code in SEXP.R for assignment to a SEXP. Same code so abstract it.
       if(FALSE) {
           i = createMultiDimGEPIndex(call, env, ir, ...)
           idx = ir$createSExt(i, 64L)
           ptr = compile(call[[2]], env, ir, ...)
           return(ir$createGEP(ptr, idx))
       } else {
           gep = createSEXPGEP(call, env, ir, ...)
           if(length(call) > 3 && any(sapply(call[-(1:2)], `==`, "")))
               return(gep)

           if(!load)
              return(gep)
     
           return(createLoad(ir, gep)) #!!! Was just createSEXPGEP(). See what this breaks! load added for matrixSubsetCmp.R
                # Check matrixSubset.R
       }
   } else if(is(dimType, "NativeMatrixType")) {

     if(length(call) == 4 && is.name(call[[4]]) && as.character(call[[4]]) == "") {
            # accessing a row, i.e.  x[i, ]
         obj = getVariable(call[[2]], env, ir)
#         obj = compile(call[[2]], env, ir, ...)
#         idx = compile(call[[3]], env, ir, ..., load = FALSE)
#         ctx = getContext(env$.module)
         idx = compileMatrixOffset(call, env, ir, ..., asSEXT = TRUE)
         ans = ir$createGEP(obj, idx)
#       if(load)
#          return(ir$createLoad(ans))
#       else
            return(ans)
     } else if(is.name(call[[3]]) && as.character(call[[3]]) == "") {

     } else {  # have both i and j. So create the offset for these.
         obj = getVariable(call[[2]], env, ir)
            # does compileMatrixOffset 
         idx = compileMatrixOffset(call, env, ir, ..., asSEXT = FALSE)
         ans = ir$createGEP(obj, idx) 
     }

   } # else ArrayType.


 
   objType = getElementAssignmentContainerType(call, env)

   obj = getVariable(call[[2]], env, ir, load = TRUE) #???? for load = FALSE. Now back to TRUE. Based on fgets.Rdb.

   zeroBased = is.name(call[[3]]) && as.character(call[[3]]) %in% names(env$.zeroBased)
   
   if(!zeroBased)  {
     call[[3]] = subtractOne(call[[3]])
     call[[4]] = subtractOne(call[[4]])
   }

   # Now, matrix or data frame
   # What about > 2 way arrays.

   p = ir$createGEP(obj, idx)

   if(load)
    return(ir$createLoad(p))

    p
}



