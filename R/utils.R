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
function(type)
{
  n <- which(sapply(basic.types, function(x) sameType(x, type)))
  if (!length(n))
    return(NA)
  names(n)
}

prettyEnv =
  # Print all the goodies in an environment. Maybe give compile
  # environments a class and make this a method.
function(env)
{
  cat(sprintf("Return type: %s\n", reverseLookupType(env$.returnType)))

  cat("Parameters:\n") # no way to get type
  cat(paste(names(env$.params), collapse=", "), "\n")

  cat("Types:\n")
  for (i in seq_along(env$.types)) {
    cat(sprintf(" %s: %s\n", names(env$.types)[[i]], reverseLookupType(env$.types[[i]])))
  }
}

findVar <-
function(var, env)
{
  return(mget(as.character(var), envir=env, ifnotfound=NA))
}

# Make this a generic and have this as the default.
# Allow other people to provide their own.
findCall <-
function(call, OPS)
{
  op <- match(as.character(call), names(OPS))
  if (is.na(op))
    return(NA) # TODO Some (specified) builtins need to be passed directly,
               # i.e. as.double()
  return(OPS[[op]])
}

checkArgs <-
function(args, types, fun)
{
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

getArgs <-
function(expr, env = NULL, ir = NULL)
{
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
        } else {
            # find in the module.
          v = getGlobalVariable(env$.module, sym)
#load = FALSE  # don't load a global, just access it. ??          
          if(load && !is.null(ir))
             ir$createLoad(v)
          else
             v
        }
}




createCast =
# Add a cast instruction; should this be in Rllvm?  So far this only
# works with Int32Type, DoubleType, and DoublePtrType.  createLoad is
# used to dereference pointers (of single values - this is temporary
# and unsafe in some cases).  FIXME - XXX
function(ir, toType, fromType, val)
{

  # The logic seems to be off here. We have to find
  # use both the from and to types rather than
  # matching on the from type and then picking a function.

  if (sameType(toType, fromType))
    stop("No need to cast: toType and fromType are same.")


  if(isIntegerType(fromType))
    return(createCastIntType(ir, val, toType, fromType))
  
  toTypes <- c(Int32Type = Int32Type,
               DoubleType = DoubleType,
               DoubleType = DoubleType)
  
  fromTypes <- c(DoubleType = DoubleType,
                 Int32Type = Int32Type,
                 DoubleType = DoublePtrType)
  
  casters <- c(createFPToSI,
               createSIToFP,
               function(ir, val, ...) createLoad(ir, val))

  i <- which(sapply(fromTypes, function(x) sameType(fromType, x)))

  if (!length(i))
    stop(sprintf("Don't know how to handle this fromType (reverseLookupType says type '%s)", reverseLookupType(fromType)))

#XXX    
 browser()

  ## checking needed here
  fun = casters[[i]]
  ins = fun(ir, val, toType)
  return(ins)
}

createCastIntType =
function(ir, val, toType, fromType, ...)
{
  if(sameType(toType, DoubleType))
    return(createSIToFP(ir, val, toType))

  w = getIntegerBitWidth(fromType)
  if(w == 1)
  return(ir$createZExt(val, toType))      

  return(ir$createIntCast(val, toType))  
#  return(ir$createBitCast(val, toType))
}


