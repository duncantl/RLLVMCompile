
multiSubset =
    # lots of overlapping/common code with subsetHandler. Consolidate.
    #
function(call, env, ir, ..., load = TRUE, SEXPToPrimitive = TRUE)
{
# browser()

   varName = as.character(call[[2]])
   dimType = NULL
   if(varName %in% names(env$.dimensionedTypes)) {
        dimType = env$.dimensionedTypes[[varName]]
#        r = getSEXPTypeElementAccessor(dimType@elType, env)
        dimensioned = TRUE        
   } else
        stop("need type and dimension information for ", varName)


   if(is(dimType, "DataFrameType")) {
      ee = substitute(x[[i]][j], list(x = call[[2]], i = call[[3]], j = call[[4]]))
      return(compile(ee, env, ir, ...))
   } else if(is(dimType,  "MatrixType")) {
       i = createMultiDimGEPIndex(call, env, ir, ...)
       idx = ir$createSExt(i, 64L)
       return(ir$createGEP(ptr, idx))
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
