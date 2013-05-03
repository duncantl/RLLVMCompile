getSApplyType =
  # returns NULL if not a case we would rewrite an sapply()
function(call, env, funName = as.character(call[[1]])) {
 if(funName == "sapply" &&  isSEXPType(type <- getDataType(call[[2]], env))  )
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
  #   When we compile this we need to know that INTGER() maps to  int *, etc.
  # When we find these globals before we generate code, we need to register them.
  #
  #     ty = getDataType(sym, env)
  #
function(call, vecType, returnType, addReturn = TRUE, env = NULL, ir = NULL, ...)
{
   X = call[[2]]
    # get length of the R vector.
   len = quote(n <- Rf_length(x))
   len[[3]][[2]] = X  # assume just a symbol

   #  get the raw data for the elements. Doesn't work for  character(). Need to call GET_STRING_ELT() within the loop.
   els = quote(els <- foo(x))
   els[[3]][[2]] = X
   els[[3]][[1]] = as.name(getSEXPDataAccessor(vecType))  # get the INTEGER, REAL, etc. for the type
   
   # allocate answer
   alloc = quote(r_ans <- Rf_allocVector(sexpEnum, n))
   alloc[[3]][[2]] = sexpTypeNum = getSEXPTypeNum(returnType)  # get INTSXP, REALSXP, etc.

   # create the instruction to get the element
     
   funCall = quote(tmp <- f(el))
   funCall[[3]][[1]] = call[[3]]
   if(length(call) > 3) 
      funCall[[3]][seq(3, length = length(call) - 3)] = call[-(1:3)]

   loop =
     quote(for(i in 1:n) {
             el = els[i]
             x
             r_ans[i] = tmp  # leave to the compiler make sense of this.
           })
   loop[[4]][[3]] = funCall


      # Add the type for r_ans to the compiler's known types
      # This might indicate that we have created that variable already
      # So we may need to maintain a list of the variables we have explicitly
      # allocated separately from the type information that we know ahead of time.
   if(!is.null(env)) 
     env$.types$r_ans = getSEXPType(names(sexpTypeNum))
   
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



