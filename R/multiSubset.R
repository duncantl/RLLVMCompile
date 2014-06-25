
multiSubset =
    # lots of overlapping/common code with subsetHandler. Consolidate.
    #
function(call, env, ir, ..., load = TRUE, SEXPToPrimitive = TRUE)
{
 browser()
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
