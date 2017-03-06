getSApplyType =
  # returns NULL if not a case we would rewrite an sapply()
    # The intent here is when we are using the apply() idiom on an non-R object
    # just for expressiveness.
function(call, env, funName = as.character(call[[1]])) {
 if(funName %in% c("sapply", "lapply") &&  isSEXPType(type <- getDataType(call[[2]], env))  )
    type
 else
     NULL
}

rewriteSApply = 
  #
  #  This rewrites a call to sapply() into a for loop, allocating the space for the answer, etc.
  #  This is used when the type of the first argument to the sapply() call is a SEXP and not
  #  a native array/pointer.
  #
  #  The approach below doesn't compile the code directly, but
  #  rewrites the code so that we can use the existing facilities to compile it.  
  #
  #  See fgets.Rdb
  #
  # For now, ignore simplify and USE.NAMES.  Assume they aren't  there.
  #
  #  If we know the types of the vector and the return type of the function,
  #  we can insert them now. Otherwise, we could use special functions that our
  #  "compiler"  will know to replace
  #
  #   When we compile this we need to know that INTEGER() maps to  int *, etc.
  # When we find these globals before we generate code, we need to register them.
  #
  #     ty = getDataType(sym, env)
  #
    #
    #  unprotect the variable, not by count (1L)
    #  [test] when the R object being returned has direct element accessor (logical, integern, numeric)
    #       get the pointer for the array outside of the loop and use it.
    #
function(call, vecType, returnType, addReturn = TRUE, env = NULL, ir = NULL, ...)
{
    # returnType is the C-level return type of the function being sapply()'ed to each element, i.e. the FUN
  

   X = call[[2]]
    # get length of the R vector.
   len = quote(n <- Rf_length(x))
   len[[3]][[2]] = X  # assume just a symbol


   hasElsAccessor = !( is(vecType, "STRSXPType") || is(vecType, "VECSXPType"))

   if(hasElsAccessor) {
                   #  get the raw data for the elements. Doesn't work for  character(). Need to call GET_STRING_ELT() within the loop.
       els = quote(els <- foo(x))
       els[[3]][[2]] = X
       els[[3]][[1]] = as.name(getSEXPDataAccessor(vecType))  # get the INTEGER, REAL, etc. for the type
   } else {
       els = NULL
   }

#XXX Could add the types for els and el right now.
   
      # allocate answer
   alloc = quote(r_ans <- Rf_allocVector(sexpEnum, n))
   alloc[[3]][[2]] = sexpTypeNum = getSEXPTypeNum(returnType) # !!returnType)  # get INTSXP, REALSXP, etc.

      # create the instruction to get the element from X and assign it to el

   funCall = quote(tmp <- f(el))
   funCall[[3]][[1]] = call[[3]]
   if(length(call) > 3)    # dealing with extra arguments to our f()
      funCall[[3]][seq(3, length = length(call) - 3)] = call[-(1:3)]

   loop =
     quote(for(i in 1:n) {
             el = els[i]
             x  # replaced with f(el)
             r_ans[i] = tmp  # leave to the compiler make sense of this.
           })
   loop[[4]][[3]] = funCall 


      # Add the type for r_ans to the compiler's known types
      # This might indicate that we have created that variable already
      # So we may need to maintain a list of the variables we have explicitly
      # allocated separately from the type information that we know ahead of time.
   R.returnType = getSEXPType(names(sexpTypeNum))
   if(!is.null(env))
            #??? would class(vecType) work the same with the new classes
     env$.types$r_ans = R.returnType

   if(!hasElsAccessor) {
       accessorFun = if(is(vecType, "STRSXPType")) "STRING_ELT" else "VECTOR_ELT"
       loop[[4]][[2]][[3]] =  substitute( f(x, i-1L), list( f = as.name(accessorFun), x = X ) )
   }

      # Fix up the assignment of the value tmp within the loop to the r_ans object
   if(is(R.returnType, "VECSXPType") || is(R.returnType, "STRSXPType")) {
       loop[[4]][[3]] = substitute(OP(r_ans, i - 1L, val), list(val = loop[[4]][[3]][[3]],
                                                                OP = if(is(R.returnType, "VECSXPType"))
                                                                        as.name("SET_VECTOR_ELT")
                                                                     else
                                                                        as.name("SET_STRING_ELT")))
   if(sameType(returnType, StringType))
       loop[[4]][[3]][[4]] = substitute(mkChar(val), list(val = loop[[4]][[3]][[4]]))
       loop[[4]] = loop[[4]][-4]       
   } else {
       acc = getSEXPTypeElementAccessor(R.returnType)
       alloc = list(alloc, substitute(r_ans_p <- ACC(r_ans), list(ACC = as.name(acc))))
       loop[[4]][[4]][[2]] = quote( r_ans_p[i] )
   }

   ans = c(len,
           els,
           alloc,
           quote(Rf_protect(r_ans)),
           loop,
           quote(Rf_unprotect(1L)),
           if(addReturn)
               quote(return(r_ans))
          )

}



