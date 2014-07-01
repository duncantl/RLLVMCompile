getGlobals =
    #
    # This is like codetools::findGlobals() but can be made aware of
    # function calls which we want to ignore because they may refer to 
    # global variables that are in an entirely different scope, e.g. R.
    # This is for compiling callbacks to R and identifying variables that
    # are to be resolved in R.
    #
    # This is not the same as findGlobals (yet!), and is probably not as comprehensive
    # but also finds some errors, i.e. finds use of variables before they are defined/assigned.
    #
    # We didn't use the code walker mechanism in codetools as it doesn't appear to give us a means
    # to push and pop function calls. But this could be just that I haven't wrapped my head around it.
    #
    # Todo:  process nested function definitions and determine their local variables
    #
    #
    #
    # When run on compileFunction(), we find ir and nenv as globals.
    # nenv is real as it used before it is defined.
    #  ir appears as a global because its is referenced in a function definition
    #  not call.
    #
function(f, expressionsFor = character(), localVars = character(),
          skip = c(".R", ".typeInfo", ".signature", ".pragma"))
{
  vars = character()
  funs = character()
  varsByFun = list()

  curFuns = character()
  expressions = list()

  subFunInfo = list()

  addFunName = function(id) {
                   funs <<- c(funs, as.character(id))
               }
  
  fun = function(e, w) {
      popFuns = FALSE
      if(is.name(e) && as.character(e) == "")  # typically a formal argument that has no default value.
          return(FALSE)

      if(is.call(e)) {
           if(is.call(e[[1]])) { # e.g. x$bob()
               return(lapply(e, fun,  w))
           } 
          
           funName = as.character(e[[1]])
          if(funName == "function") {
             subFunInfo[[length(subFunInfo)+1L]] <<- getGlobals(e, expressionsFor)
          return(TRUE)
          } else if(funName %in% c('<-', '=')) {
             if(is.name(e[[2]]))
                localVars <<- c(localVars, as.character(e[[2]]))
          } else {
             if(funName %in% skip)
                 return(TRUE)
              
             if(length(curFuns))
                 sapply(curFuns, function(id)
                                    varsByFun[[id]] <<- c(varsByFun[[id]], funName))
              curFuns <<- c(curFuns, funName)
              funs <<- c(funs, funName)
              popFuns = TRUE

             if(funName %in% expressionsFor)
                 expressions[[ funName ]] <<- c(expressions[[ funName ]], e)
          }

          els = as.list(e)[-1]
          if(funName == "$") # remove the literal name
             els = els[-2]
           else if(funName %in% c("sapply", "lapply") && is.name(e[[3]])) {
              addFunName(as.character(e[[3]])) #XXX Also need to add it to the varsByFuns
              els = els[-2]
           }
           
          lapply(els, fun, w)
          if(popFuns)
              curFuns <<- curFuns[- length(curFuns)]
      } else if(is.name(e) && !(as.character(e) %in% localVars)) {
             name = as.character(e)
             if(!(name %in% localVars) && length(curFuns) && any(expressionsFor %in% curFuns)) {
             } else
                vars <<- c(vars, name)
             
             if(length(curFuns))
                 sapply(curFuns, function(id)
                                    varsByFun[[id]] <<- c(varsByFun[[id]], name))
        } else
          lapply(as.list(e)[-1], fun, w)
  }

  if(is.call(f) && as.character(f[[1]]) == "function") 
     f = eval(f, globalenv())

  if(typeof(f) == "closure") {
      localVars = names(formals(f))
      lapply(formals(f), fun, fun)
      f = body(f)
  } 
  
  fun(f, fun)

  varsByFun = varsByFun[ setdiff(names(varsByFun), c("for", "if", "{"))]

    # This doesn't catch the case that a function is defined and called before the variable it references in the parent function
    # is defined.

  gvs = unlist(sapply(subFunInfo, `[[`, "variables"))
  i = match(gvs, localVars)
  vars = c(vars, gvs[is.na(i)])
  list(localVariables = localVars,
       variables = vars,
       functions = funs,
       variablesByFun = lapply(varsByFun, table),
       expressions = expressions,
       subFunctions = subFunInfo)
}

