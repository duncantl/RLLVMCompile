
basic.types = substitute(c(Int16Type, Int1Type, Int32PtrType,
  Int32Type, Int64Type, Int8Type, VoidType,
  FloatPtrType, FloatType, DoublePtrType, DoubleType))

btnames = sapply(basic.types[-1], deparse)
basic.types = sapply(basic.types[-1], eval)
names(basic.types) = btnames

reverseLookupType =
# Given an external pointer, find the corresponding name for this
# type. This is useful for debugging when Rllvm crashes and printing
# everything in an environment.
function(type) {
  n <- which(sapply(basic.types, function(x) identical(x, type)))
  if (!length(n))
    return(NA)
  names(n)
}

prettyEnv =
# Print all the goodies in an environment. Maybe give compile
# environments a class and make this a method.
function(env) {
  cat(sprintf("Return type: %s\n", reverseLookupType(env$.returnType)))

  cat("Parameters:\n") # no way to get type
  cat(paste(names(env$.params), collapse=", "), "\n")

  cat("Types:\n")
  for (i in seq_along(env$.types)) {
    cat(sprintf(" %s: %s\n", names(env$.types)[[i]], reverseLookupType(env$.types[[i]])))
  }
}

findVar <- function(var, env) {
  return(mget(as.character(var), envir=env, ifnotfound=NA))
}

# Make this a generic and have this as the default.
# Allow other people to provide their own.
findCall <- function(call, OPS)
{
  op <- match(as.character(call), names(OPS))
  if (is.na(op))
    return(NA) # TODO Some (specified) builtins need to be passed directly,
               # i.e. as.double()
  return(OPS[[op]])
}

checkArgs <- function(args, types, fun) {
  # for side-effect of stop() when wrong arg type
  # encountered. TypeInfo will replace this probably.
  if (length(args) != length(types))
    stop("Wrong number of args provided.")
  for (i in seq_along(args)) {
    if (types[[i]] != 'ANY' && !(typeof(args[[i]]) %in% types[[i]]))
      stop(fun, " received an argument of the wrong type\n",
           "expected any of:\n", paste(types[[i]], sep=', ', collapse=', '),
           "\ngot: ", typeof(args[[i]]))
  }
}

getArgs <- function(expr, env = NULL, ir = NULL) {
  # Converts to list; TODO this is no longer used by any code -
  # candidate for removal
  if(typeof(expr) != "language")
     stop("expr must be of type 'language' in getArgs")
  ans = sapply(expr[-1], function(x) x)
  if(!is.null(env))
    lapply(ans, compile, env, ir)
  else
    ans
}


## isNumericConstant <- function(expr) {
##   # TODO no complex cases yet
##   if (class(expr) %in% c('numeric', 'integer'))
##     return(TRUE)
##   return(FALSE)
## }


getVariable =
function(sym, env, ir = NULL, load = TRUE, search.params=TRUE, ...)
{

  sym = as.character(sym)
  var = if(exists(sym, env)) {
               # The local variables we create in the function
               # are alloc'ed and so are pointers. They need to be
               # loaded to use their values.
          tmp = get(sym, env)
          if(load && !is.null(ir))
             ir$createLoad(tmp)
          else
            tmp
        } else if(search.params && sym %in% names(env$.params)) {
          env$.params[[sym]]
        } else
            # find in the module.
          getGlobalVariable(env$.module, sym)
}



insertReturn =
  # Checks to see if we need to enclose the final expression
  # within a call to return()
  #
  # insertReturn(quote(return(x + 1))  )
  # insertReturn(quote(x + 1))
  # insertReturn(quote({ x = 2; x + 1} ))
  # insertReturn(quote({ x = 2; return(x + 1)} ))
  # insertReturn(quote(while(TRUE) {  return(x + 1) }  ))
  # insertReturn(quote(while(TRUE) {  x + 1 }  ))
  # insertReturn(quote(if(x < 10) 20 else 40  ))
  # insertReturn(quote(if(x < 10) { x= 3; sqrt(x) } else 40  ))
  # insertReturn(quote(if(x < 10) { x= 3; sqrt(x) } else { x = 100; sqrt(x)}  ))      
  #
  #XXX Need to handle while, if  
function(expr, nested = FALSE, ...)
  UseMethod("insertReturn")


`insertReturn.{` =
function(expr, nested = FALSE, ...)
{
     expr[[length(expr)]] = insertReturn(expr[[length(expr)]], nested)
     expr
}


`insertReturn.call` =
function(expr, nested = FALSE, ...)
{
  if(nested || expr[[1]] != as.name('return')) {
    if(nested) {
        # create .ret = expr
      k = quote(.ret <- val)
      k[[3]] = expr
    } else {
      k = call('return')
      k[[2]] = expr
    }
    k
  } else
    expr
}

insertReturn.if =
function(expr, nested = FALSE, ...)
{
  expr[[3]] = insertReturn(expr[[3]], nested = TRUE)
  if(length(expr) == 4)
      expr[[4]] = insertReturn(expr[[4]], nested = TRUE)
  expr
}

insertReturn.numeric = insertReturn.logical = insertReturn.character =
  insertReturn.integer = `insertReturn.(` =     # should check for (return(x+1))
   function(expr, nested = FALSE, ...) {
     k = call('return')
     k[[2]] = expr
     k
   }


insertReturn.while =
function(expr, nested = FALSE, ...)
{
  expr[[3]] = insertReturn(expr[[3]], nested = TRUE)
  expr
}
