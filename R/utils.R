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
  if (is.na(op)) {
    # warning("did not find ", as.character(call), " in ops")
    return(NA) # TODO Some (specified) builtins need to be passed directly,
               # i.e. as.double()
  }
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
  ans = sapply(expr[-1], function(x) x) # why not just expr[-1] ??
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
function(sym, env, ir = NULL, load = TRUE, search.params=TRUE, searchR = FALSE, ...)
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
        } else if(!is.null(v <- getGlobalVariable(env$.module, sym)) || searchR == FALSE) {            # find in the module.
#load = FALSE  # don't load a global, just access it. ??          
          if(load && !is.null(ir) && ! (isPointerType(getType(v)) && isArrayType(getElementType(getType(v)))))
             ir$createLoad(v)
          else
             v
        } else if(exists(sym)) {
            # Look for a simple literal, constant value in R
           tmp = get(sym)
           
           if(is.atomic(tmp)) {
               v = ir$createConstant(tmp)
#segfaults.  Do we need to load the constant? Probably not.               
#               if(load && !is.null(ir))
#                   ir$createLoad(v)
#               else
                   v
           } else
               stop("found ", sym, " in R, but it is not an atomic value")
        } else
             stop("cannot find variable named ", sym)
}




createCast =
# Add a cast instruction; should this be in Rllvm?  So far this only
# works with Int32Type, DoubleType, and DoublePtrType.  createLoad is
# used to dereference pointers (of single values - this is temporary
# and unsafe in some cases).  FIXME - XXX
function(env, ir, toType, fromType, val, ...)
{
  # The logic seems to be off here. We have to find
  # both the from and to types rather than
  # matching on the from type and then picking a function.

  if (sameType(toType, fromType))
    stop("No need to cast: toType and fromType are same.")


  if(sameType(fromType, STRSXPType) && sameType(toType, StringType)) {
       # Really need to check is CHARSXP. We could be dealing with any SXPType
      e = substitute(R_CHAR(x), list(x = val))
      return(compile(e, env, ir, ...))
  }
  

   # convert from Double to Float.
  if(sameType(toType, FloatType) && sameType(fromType, DoubleType))
    return(ir$createFPTrunc(val, toType))

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

  ## checking needed here
  fun = casters[[i]]
  ins = fun(ir, val, toType)
  return(ins)
}

createCastIntType =
function(ir, val, toType, fromType, ...)
{
  if(sameType(toType, DoubleType) || sameType(toType, FloatType))
      return(createSIToFP(ir, val, toType))

  w.from = getIntegerBitWidth(fromType)
  if(isIntegerType(toType)) {
     w.to = getIntegerBitWidth(toType)
     if(w.to > w.from)
       return(ir$createZExt(val, w.to))
     else
       return(ir$createTrunc(val, toType)) 
  }

# This was here for some reason. (July 15th, 2013)
#  if(w.from == 1)
#     return(ir$createZExt(val, toType))      

  return(ir$createIntCast(val, toType))  
#  return(ir$createBitCast(val, toType))
}


