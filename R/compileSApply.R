
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
function(call, vecType, returnType, env = NULL, ir = NULL, ...)
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
   alloc = quote(r_ans <- Rf_allocVector(n, sexpEnum))
   alloc[[3]][[3]] = getSEXPTypeNum(returnType)  # get INTSXP, REALSXP, etc.

   # create the instruction to get the element
     
   funCall = quote(tmp <- f(el))
   funCall[[3]][[1]] = call[[3]]
   if(length(call) > 3) 
      funCall[[3]][seq(3, length = length(call) - 3)] = call[-(1:3)]

   loop =
     quote(for(i in 1:n) {
             el = els[i]
             x
             r_ans[i] = el  # leave to the compiler make sense of this.
           })
   loop[[4]][[3]] = funCall

   ans = c(len,
           els,
           alloc,
           quote(Rf_protect(r_ans)),
           loop,
           quote(Rf_unprotect(1)),
           quote(return(r_ans)))

}

getSEXPDataAccessor =
function(type)
{
   if(sameType(type,  getSEXPType("INT")))  
      "INTEGER"
   else if(sameType(type, getSEXPType("LGL"))) 
      "INTEGER"   
   else if(sameType(type, getSEXPType("REAL")))
      "REAL"
   else
     stop("problem getting R data accessor routine name")
}

STRSXP = 16L
LGLSXP = 9L
REALSXP = 14L
INTSXP = 13L
ANYSXP = 18L
CHARSXP = 9L

getSEXPTypeNum =
function(type)
{
  if(sameType(type, getSEXPType("STR")))
     STRSXP
  else if(sameType(type, getSEXPType("REAL")))
     REALSXP
  else if(sameType(type, getSEXPType("LGL")))
     LGLSXP
  else if(sameType(type, getSEXPType("INTSXP")))
     INTSXP
  else
    stop("...")
}
